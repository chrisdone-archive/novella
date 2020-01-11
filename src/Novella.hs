{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

-- | The Novella structured editor.

module Novella
  ( commandParser
  , commandConduit
  , transformState
  ) where

import           Control.Monad.Trans
import           Control.Monad.Trans.Reader (ask, ReaderT)
import           Control.Monad.Trans.State.Strict (modify, StateT)
import           Control.Monad.Writer
import           Data.Conduit
import           Data.Foldable
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Reparsec (failWith, ParserT)
import qualified Data.Reparsec.Sequence as Reparsec
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Novella.Types
import           Novella.Types.Optics
import           Optics
import           RIO (glog)

--------------------------------------------------------------------------------
-- Low-level command parser

-- | Produce a stream of commands from a stream of inputs.
commandConduit ::
     MonadIO m
  => Config -> ConduitT Input (Either CommandParseError Command) (ReaderT State m) ()
commandConduit config = Reparsec.parseConduit (commandParser config)

-- | Produce a command from a stream of inputs.
commandParser ::
     MonadIO m => Config -> ParserT (Seq Input) CommandParseError (ReaderT State m) Command
commandParser config = quit <> ctrlc <> parserFromState config
  where quit = QuitCommand <$ Reparsec.expect EscInput
        ctrlc = CtrlCCommand <$ Reparsec.expect CtrlInput <* Reparsec.expect (CharInput 'c')

--------------------------------------------------------------------------------
-- Command parser based on current node's schema

-- | Parse commands based on the current selected node.
parserFromState ::
     MonadIO m
  => Config
  -> ParserT (Seq Input) CommandParseError (ReaderT State m) Command
parserFromState config = do
  state <- lift ask
  case listToMaybe
         (execWriter
            (traverseOf
               typedSlotTraversalAtCursor
               (\x -> do
                  tell [x]
                  pure (_typedSlotSlot x))
               state)) of
    Nothing -> failWith NoCurrentFocusedNode
    Just typedSlot -> do
      lift (glog (CommandParserMsg (ParsingForSlot typedSlot)))
      case _typedSlotSlot typedSlot of
        FilledSlot {} -> failWith NoActionForFilledSlot
        QuerySlot query ->
          case M.lookup schemaName (configSchema config) of
            Nothing -> failWith (NoSuchSchemaToQuery schemaName)
            Just schema -> do
              lift (glog (CommandParserMsg (FoundSchema schema)))
              case schema of
                KeywordSchema keyword ->
                  failWith (QueryingKeywordSchema keyword)
                ChoiceSchema schemaNames ->
                  parseQueryUpdate query (matchChoiceSchema (configSchema config) schemaNames)
                _ -> failWith NoNodeMatches
      where schemaName = _typedSlotSchema typedSlot

-- | Parse keys upon the current query, producing a command.
parseQueryUpdate ::
     Monad m
  => Query
  -> (Query -> Seq Match)
  -> ParserT (Seq Input) CommandParseError (ReaderT State m) Command
parseQueryUpdate query matches = do
  input <- Reparsec.nextElement
  query' <-
    case input of
      CharInput ch -> pure (over queryText (<> [ch]) query)
      UpInput -> pure (over querySelection pred query)
      DownInput -> pure (over querySelection succ query)
      BackspaceInput -> pure (over queryText (reverse . drop 1 . reverse) query)
      _ -> failWith UnknownCommand
  pure (UpdateQuery (query' {_queryMatches = matches query'}))

--------------------------------------------------------------------------------
-- Matching against schemas

matchChoiceSchema :: Map SchemaName Schema -> NonEmpty SchemaName -> Query -> Seq Match
matchChoiceSchema rules schemas query =
  mconcat (fmap (matchShallowSchema rules query) (toList schemas))

matchShallowSchema :: Map SchemaName Schema -> Query -> SchemaName -> Seq Match
matchShallowSchema rules query schemaName =
  case M.lookup schemaName rules of
    Nothing -> mempty
    Just schema ->
      if null (_queryText query)
         then pure (Match schemaName)
         else case schema of
                CompositeSchema schemas ->
                  if any (not . Seq.null . matchAtomicSchema rules query) schemas
                    then pure (Match schemaName)
                    else mempty
                _ -> matchAtomicSchema rules query schemaName

matchAtomicSchema :: Map SchemaName Schema -> Query -> SchemaName -> Seq Match
matchAtomicSchema rules (Query {_queryText = text}) schemaName =
  case M.lookup schemaName rules of
    Nothing -> mempty
    Just schema ->
      case schema of
        KeywordSchema (Keyword kw) ->
          if isInfixOf text kw
            then pure (Match schemaName)
            else mempty
        _ -> mempty

--------------------------------------------------------------------------------
-- State transformer of commands

-- | Transform the state given the config and the command.
transformState :: Monad m => Config -> Command -> StateT State m Loop
transformState _config =
  \case
    QuitCommand -> pure ExitLoop
    EXITCommand -> pure ExitLoop
    CtrlCCommand -> pure ExitLoop
    UpdateQuery query -> do
      modify
        (set (typedSlotTraversalAtCursor % typedSlotSlotNode % _QuerySlot) query)
      pure ContinueLoop
