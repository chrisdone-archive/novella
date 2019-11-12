{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Brick frontend for Novella.

module Novella.Brick where

import qualified Brick
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Validation
import qualified Graphics.Vty as Vty
import           Novella.Types

data NovellaState = NovellaState
  { inputs :: !(Seq Input)
  }

data NovellaEvent =
  StartEvent

data NovellaResource = NovellaResource
  deriving (Ord, Eq)

app :: Brick.App NovellaState NovellaEvent NovellaResource
app =
  Brick.App
    { appDraw = draw
    , appChooseCursor = \_state [] -> Nothing
    , appHandleEvent = handleEvent
    , appStartEvent = \_state -> pure NovellaState {inputs = mempty}
    , appAttrMap = const (Brick.attrMap Vty.defAttr [])
    }

draw :: p -> [Brick.Widget NovellaResource]
draw _state = [ui]

-- | Handle an incoming event.
handleEvent ::
     NovellaState
  -> Brick.BrickEvent NovellaResource NovellaEvent
  -> Brick.EventM NovellaResource (Brick.Next NovellaState)
handleEvent state event =
  case event of
    Brick.VtyEvent (Vty.EvKey key modifiers) -> handleEvKey state key modifiers
    _ -> Brick.continue state

-- | Handle incoming keys.
handleEvKey ::
     NovellaState
  -> Vty.Key
  -> [Vty.Modifier]
  -> Brick.EventM n (Brick.Next NovellaState)
handleEvKey state key modifiers =
  case traverse modifierToInput (Seq.fromList modifiers) of
    Failure _badModifiers -> Brick.continue state -- TODO: display bad keys.
    Success modifierInputs ->
      case keyToInput key of
        Left _badKey -> Brick.continue state -- TODO: display problem.
        Right keyInput ->
          Brick.continue
            state {inputs = inputs state <> modifierInputs <> pure keyInput}

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
