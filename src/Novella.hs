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
import           Data.List.NonEmpty (NonEmpty(..))
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
commandParser config = quit <> ctrlc <> downSelect <> upSelect <> parserFromState config
  where quit = QuitCommand <$ Reparsec.expect EscInput
        -- exit = EXITCommand <$ traverse (Reparsec.expect . CharInput) "exit"
        ctrlc = CtrlCCommand <$ Reparsec.expect CtrlInput <* Reparsec.expect (CharInput 'c')
        downSelect = DownCommand <$ Reparsec.expect DownInput
        upSelect = UpCommand <$ Reparsec.expect UpInput

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
                  pure (SetMatches (matchChoiceSchema schemaNames query))
                _ -> failWith NoNodeMatches
      where schemaName = _typedSlotSchema typedSlot

--------------------------------------------------------------------------------
-- Matching against schemas

matchChoiceSchema :: NonEmpty SchemaName -> Query -> Seq Match
matchChoiceSchema schemas _query = fmap Match (Seq.fromList (toList schemas))

--------------------------------------------------------------------------------
-- State transformer of commands

-- | Transform the state given the config and the command.
transformState :: Monad m => Config -> Command -> StateT State m Loop
transformState _config =
  \case
    QuitCommand -> pure ExitLoop
    EXITCommand -> pure ExitLoop
    CtrlCCommand -> pure ExitLoop
    DownCommand -> do
      modify
        (over
           (typedSlotTraversalAtCursor % typedSlotSlotNode % _QuerySlot %
            querySelection)
           succ)
      pure ContinueLoop
    UpCommand -> do
      modify
        (over
           (typedSlotTraversalAtCursor % typedSlotSlotNode % _QuerySlot %
            querySelection)
           pred)
      pure ContinueLoop
    SetMatches matches -> do
      modify
        (set
           (typedSlotTraversalAtCursor % typedSlotSlotNode % _QuerySlot %
            queryMatches)
           matches)
      pure ContinueLoop
