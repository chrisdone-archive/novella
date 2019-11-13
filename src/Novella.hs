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

-- | Produce a stream of commands from a stream of inputs.
commandConduit ::
     Monad m
  => ConduitT Input (Either CommandParseError Command) (ReaderT State m) ()
commandConduit = Reparsec.parseConduit commandParser

-- | Produce a command from a stream of inputs.
commandParser ::
     Monad m => ParserT (Seq Input) CommandParseError (ReaderT State m) Command
commandParser = quit <> exit <> ctrlc <> downSelect
  where quit = QuitCommand <$ Reparsec.expect EscInput
        exit = EXITCommand <$ traverse (Reparsec.expect . CharInput) "exit"
        ctrlc = CtrlCCommand <$ Reparsec.expect CtrlInput <* Reparsec.expect (CharInput 'c')
        downSelect = DownCommand <$ Reparsec.expect DownInput

-- | Transform the state given the config and the command.
transformState :: Monad m => Config -> Command -> StateT State m Loop
transformState _config =
  \case
    QuitCommand -> pure ExitLoop
    EXITCommand -> pure ExitLoop
    CtrlCCommand -> pure ExitLoop

-- Most of the code below this line can be rewritten with lenses and prisms and/or traversals.

    DownCommand -> do
      modify
        (modifyTypedSlotAtCursorInState
           (\typedSlot ->
              case typedSlotSlot typedSlot of
                QuerySlot query ->
                  typedSlot
                    { typedSlotSlot =
                        QuerySlot
                          (query {querySelection = querySelection query + 1})
                    }
                _ -> typedSlot))
      pure ContinueLoop

stateTypedSlotAtCursor :: State -> TypedSlot
stateTypedSlotAtCursor = stateTypedSlot

modifyTypedSlotAtCursorInState :: (TypedSlot -> TypedSlot) -> State -> State
modifyTypedSlotAtCursorInState f state =
  state
    { stateTypedSlot =
        modifyTypedSlotAtCursor f (stateCursor state) (stateTypedSlot state)
    }

modifyTypedSlotAtCursor f Here typedSlot = f typedSlot
