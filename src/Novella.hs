{-# LANGUAGE Strict #-}
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
import           Data.Maybe
import           Data.Reparsec (failWith, ParserT)
import qualified Data.Reparsec.Sequence as Reparsec
import           Data.Sequence (Seq)
import           Novella.Types
import           Novella.Types.Optics
import           Optics
import           RIO (glog)

--------------------------------------------------------------------------------
-- Low-level command parser

-- | Produce a stream of commands from a stream of inputs.
commandConduit ::
     MonadIO m
  => ConduitT Input (Either CommandParseError Command) (ReaderT State m) ()
commandConduit = Reparsec.parseConduit commandParser

-- | Produce a command from a stream of inputs.
commandParser ::
     MonadIO m => ParserT (Seq Input) CommandParseError (ReaderT State m) Command
commandParser = quit <> ctrlc <> downSelect <> upSelect <> parserFromState
  where quit = QuitCommand <$ Reparsec.expect EscInput
        -- exit = EXITCommand <$ traverse (Reparsec.expect . CharInput) "exit"
        ctrlc = CtrlCCommand <$ Reparsec.expect CtrlInput <* Reparsec.expect (CharInput 'c')
        downSelect = DownCommand <$ Reparsec.expect DownInput
        upSelect = UpCommand <$ Reparsec.expect UpInput

--------------------------------------------------------------------------------
-- Command parser based on current node's schema

-- | Parse commands based on the current selected node.
parserFromState ::
     MonadIO m => ParserT (Seq Input) CommandParseError (ReaderT State m) Command
parserFromState = do
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
      failWith NoNodeMatches

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
