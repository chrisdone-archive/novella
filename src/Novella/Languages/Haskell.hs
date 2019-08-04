{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

-- | Grammar for Haskell.

module Novella.Languages.Haskell where

import Data.Map.Strict (Map)
import Novella.Define
import Novella.Types (Schema(..), SchemaName(..))

-- | Grammar for Haskell.
haskell :: (SchemaName, Map SchemaName Schema)
haskell = runDefine $ mdo
  expression       <- define "Expression" (ChoiceSchema [variable, constructor, parentheses, tuple])
  openParenSchema  <- define "Open paren" (KeywordSchema "(")
  closeParenSchema <- define "Close paren" (KeywordSchema "(")
  parentheses      <- define "Parentheses" (CompositeSchema [openParenSchema, expression, closeParenSchema])
  tuple            <- define "Tuple" (CompositeSchema [openParenSchema, tupleElements, closeParenSchema])
  tupleElements    <- define "Tuple elements" (ListSchema expression ",")
  variable         <- define "Variable" (IdentifierSchema "Variable")
  constructor      <- define "Constructor" (IdentifierSchema "Constructor")
  pure expression
