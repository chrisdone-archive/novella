{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Entry point to Brick-based frontend for novella.

module Main where

import Brick
import Data.Functor
import Novella.Brick
import qualified Novella.Languages.Haskell as Haskell
import Novella.Types

main :: IO ()
main = void (defaultMain (app config) brickState)
  where
    config = Config {configSchema = grammarRules Haskell.grammar}
    brickState = BrickState {state, partial = Nothing}
    state =
      State
        { stateTypedSlot =
            TypedSlot
              {typedSlotSlot = QuerySlot mempty, typedSlotSchema = "Expression"}
        , stateCursor = Here
        }
