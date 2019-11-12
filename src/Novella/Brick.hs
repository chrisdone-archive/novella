-- | Brick frontend for Novella.

module Novella.Brick where

import Brick
import Graphics.Vty

data NovellaState = NovellaState
  {
  }

data NovellaEvent =
  StartEvent

data NovellaResource = NovellaResource
  deriving (Ord, Eq)

app :: App NovellaState NovellaEvent NovellaResource
app =
  App
    { appDraw = draw
    , appChooseCursor = \state [] -> Nothing
    , appHandleEvent = handleEvent
    , appStartEvent = \state -> pure NovellaState
    , appAttrMap = const (attrMap Graphics.Vty.defAttr [])
    }


draw state = [ui]

handleEvent :: s -> p -> EventM n (Next s)
handleEvent state event = halt state

ui :: Widget NovellaResource
ui = str "Hello, world!"
