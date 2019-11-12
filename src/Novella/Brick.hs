{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Brick frontend for Novella.

module Novella.Brick where

import qualified Brick
import           Control.Monad.State.Strict (runState)
import           Control.Monad.Trans.Reader
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import           Data.Reparsec
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Validation
import qualified Graphics.Vty as Vty
import           Novella
import           Novella.Types

data BrickState = BrickState
  { state :: State
  , partial :: Maybe (Maybe (Seq Input) -> Reader State (Result (Reader State) (Seq Input) CommandParseError Command))
  }

data NovellaEvent =
  StartEvent

data NovellaResource = NovellaResource
  deriving (Ord, Eq)

app :: Config -> Brick.App BrickState NovellaEvent NovellaResource
app config =
  Brick.App
    { appDraw = draw
    , appChooseCursor = \_state [] -> Nothing
    , appHandleEvent = handleEvent config
    , appStartEvent = pure
    , appAttrMap = const (Brick.attrMap Vty.defAttr [])
    }

draw :: p -> [Brick.Widget NovellaResource]
draw _state = [ui]

-- | Handle an incoming event.
handleEvent ::
     Config
  -> BrickState
  -> Brick.BrickEvent NovellaResource NovellaEvent
  -> Brick.EventM NovellaResource (Brick.Next BrickState)
handleEvent config brickState event =
  case event of
    Brick.VtyEvent (Vty.EvKey key modifiers) ->
      handleEvKey config brickState key modifiers
    _ -> Brick.continue brickState

-- | Handle incoming keys.
handleEvKey ::
     Config
  -> BrickState
  -> Vty.Key
  -> [Vty.Modifier]
  -> Brick.EventM n (Brick.Next BrickState)
handleEvKey config brickState@BrickState {state, partial} key modifiers =
  case traverse modifierToInput (Seq.fromList modifiers) of
    Failure _badModifiers -> Brick.continue brickState -- TODO: display bad keys.
    Success modifierInputs ->
      case keyToInput key of
        Left _badKey -> Brick.continue brickState -- TODO: display problem.
        Right keyInput ->
          case runReader
                 (fromMaybe
                    (parseResultT commandParser)
                    partial
                    (pure (modifierInputs <> pure keyInput)))
                 state of
            Done _inputsConsumed _position _more command ->
              case runState (transformState config command) state of
                (ExitLoop, state') -> Brick.halt (brickState {state = state'})
                (ContinueLoop, state') ->
                  Brick.continue (brickState {state = state'})
            Failed _inputsConsumed _position _more _reason ->
              Brick.continue brickState {partial = Nothing} -- TODO: display reason.
            Partial cont ->
              Brick.continue brickState {partial = pure cont} -- TODO: display problem.

-- | Convert a Vty modifier to a regular Novella input.
modifierToInput :: Vty.Modifier -> Validation (NonEmpty Vty.Modifier) Input
modifierToInput =
  \case
    Vty.MShift -> pure ShiftInput
    Vty.MCtrl -> pure CtrlInput
    Vty.MAlt -> pure AltInput
    key -> Failure (pure key)

-- | Convert a Vty key to a regular Novella input.
keyToInput :: Vty.Key -> Either Vty.Key Input
keyToInput =
  \case
    Vty.KEsc -> pure (EscInput)
    Vty.KChar char -> pure (CharInput char)
    Vty.KBS -> pure (BackspaceInput)
    Vty.KEnter -> pure (EnterInput)
    Vty.KLeft -> pure (LeftInput)
    Vty.KRight -> pure (RightInput)
    Vty.KUp -> pure (UpInput)
    Vty.KDown -> pure (DownInput)
    key -> Left key

ui :: Brick.Widget NovellaResource
ui = Brick.str "Hello, world!"
