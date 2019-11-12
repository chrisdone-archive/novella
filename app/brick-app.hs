-- | Entry point to Brick-based frontend for novella.

module Main where

import Brick
import Conduit
import Data.Functor
import Data.Reparsec
import Novella.Brick

main :: IO ()
main = void (defaultMain app state)
  where state = NovellaState
