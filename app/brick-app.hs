-- | Entry point to Brick-based frontend for novella.

module Main where

import Brick
import Data.Functor
import Novella.Brick

main :: IO ()
main = void (defaultMain app state)
  where
    state = NovellaState {inputs = mempty}
