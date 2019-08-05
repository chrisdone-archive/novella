-- | A list of all supported languages.

module Novella.Languages
  ( haskell
  ) where

import           Novella.Types
import qualified Novella.Languages.Haskell as Haskell

haskell :: Grammar
haskell = Haskell.grammar
