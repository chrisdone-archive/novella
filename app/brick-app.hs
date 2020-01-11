{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Entry point to Brick-based frontend for novella.

module Main where

import           Brick
import qualified Data.ByteString.Char8 as S8
import           Data.Functor
import           Data.Time
import           Novella.Brick
import qualified Novella.Languages.Haskell as Haskell
import           Novella.Types
import           RIO

main :: IO ()
main =
  withFile
    "novella.log"
    AppendMode
    (\h -> do
       hSetBuffering h LineBuffering
       void (defaultMain (app (glogfunc h) config) brickState))
  where
    glogfunc h =
      mkGLogFunc
        (\_cs c -> do
           now <- getCurrentTime
           S8.hPutStrLn h (fromString (show now ++ ": " ++ show c)))
    config = Config {configSchema = grammarRules Haskell.grammar}
    brickState = BrickState {state, partial = Nothing}
    state =
      State
        { _stateTypedSlot =
            TypedSlot
              { _typedSlotSlot =
                  QuerySlot (Query {_queryText = "", _querySelection = 0})
              , _typedSlotSchema = "Expression"
              }
        , _stateCursor = Here
        }
