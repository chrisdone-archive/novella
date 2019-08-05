{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Mini-DSL for defining grammars.

module Novella.Define
  ( rule
  , Define
  , runDefine
  , checkGrammar
  , Grammar
  , grammarToplevel
  , grammarRules
  ) where

import           Control.Monad.State.Lazy hiding (lift)
import qualified Data.Map.Strict as M
import           Language.Haskell.TH.Syntax
import           Novella.Types (Schema(..), SchemaName(..), Grammar(..))

-- | Define the grammar, returning the top-level production.
runDefine :: Define SchemaName -> (SchemaName, [(SchemaName, Schema)])
runDefine = flip runState mempty . unDefine

-- | A defining monad.
newtype Define a =
  Define
    { unDefine :: State [(SchemaName, Schema)] a
    }
  deriving (Monad, Functor, Applicative, MonadFix)

-- | Define a grammar production rule.
rule :: String -> Schema -> Define SchemaName
rule name schema =
  Define
    (do modify ((SchemaName name, schema) :)
        pure (SchemaName name))

-- | Check the grammar spec to produce a grammar.
checkGrammar :: (SchemaName, [(SchemaName, Schema)]) -> Q Exp
checkGrammar (toplevel, rules) =
  if M.size rulesMap /= length rules
    then error "Duplicate rule names in grammar."
    else lift (Grammar {grammarToplevel = toplevel, grammarRules = rulesMap})
  where
    rulesMap = M.fromList rules
