{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}

-- | The Novella structured editor.

module Novella
  ( commandParser
  , commandConduit
  , transformState
  ) where

import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.State.Strict (modify, StateT)
import           Data.Conduit
import           Data.Reparsec (ParserT)
import qualified Data.Reparsec.Sequence as Reparsec
import           Data.Sequence (Seq)
import           Novella.Types
import           Optics

-- | Produce a stream of commands from a stream of inputs.
commandConduit ::
     Monad m
  => ConduitT Input (Either CommandParseError Command) (ReaderT State m) ()
commandConduit = Reparsec.parseConduit commandParser

-- | Produce a command from a stream of inputs.
commandParser ::
     Monad m => ParserT (Seq Input) CommandParseError (ReaderT State m) Command
commandParser = quit <> exit <> ctrlc <> downSelect <> upSelect
  where quit = QuitCommand <$ Reparsec.expect EscInput
        exit = EXITCommand <$ traverse (Reparsec.expect . CharInput) "exit"
        ctrlc = CtrlCCommand <$ Reparsec.expect CtrlInput <* Reparsec.expect (CharInput 'c')
        downSelect = DownCommand <$ Reparsec.expect DownInput
        upSelect = UpCommand <$ Reparsec.expect UpInput

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
           (stateTypedSlot % typedSlotSlot % _QuerySlot % querySelection)
           succ)
      pure ContinueLoop
    UpCommand -> do
      modify
        (over
           (stateTypedSlot % typedSlotSlot % _QuerySlot % querySelection)
           pred)
      pure ContinueLoop
