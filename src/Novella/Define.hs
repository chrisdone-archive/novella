{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Mini-DSL for defining grammars.

module Novella.Define
  ( define
  , Define
  , runDefine
  ) where

import           Control.Monad.State.Lazy
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Novella.Types (Schema(..), SchemaName(..))

-- | Define the grammar, returning the top-level production.
runDefine :: Define SchemaName -> (SchemaName, Map SchemaName Schema)
runDefine = flip runState mempty . unDefine

-- | A defining monad.
newtype Define a =
  Define
    { unDefine :: State (Map SchemaName Schema) a
    }
  deriving (Monad, Functor, Applicative, MonadFix)

-- | Define a grammar production rule.
define :: String -> Schema -> Define SchemaName
define name schema =
  Define
    (do modify (M.insert (SchemaName name) schema)
        pure (SchemaName name))
