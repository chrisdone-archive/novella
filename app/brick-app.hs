{-# LANGUAGE OverloadedStrings #-}

-- | Entry point to Brick-based frontend for novella.

module Main where

import Brick
import Data.Functor
import Novella.Brick
import qualified Novella.Languages.Haskell as Haskell
import Novella.Types

main :: IO ()
main = void (defaultMain (app config) state)
  where
    config = Config {configSchema = grammarRules Haskell.grammar}
    state =
      State
        { stateTypedSlot =
            TypedSlot
              {typedSlotSlot = EmptySlot, typedSlotSchema = "Expression"}
        , stateCursor = Here
        }
