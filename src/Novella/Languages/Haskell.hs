{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

-- | Grammar for Haskell.

module Novella.Languages.Haskell
  ( grammar
  ) where

import Instances.TH.Lift ()
import Novella.Define
import Novella.Types (Schema(..))

-- | Grammar for Haskell.
grammar :: Grammar
grammar = $(checkGrammar $ runDefine $ mdo
  expression       <- define "Expression" (ChoiceSchema [variable, constructor, parentheses, tuple, let', application])
  application      <- define "Application" (CompositeSchema [expression, expression])
  openParenSchema  <- define "Open paren" (KeywordSchema "(")
  closeParenSchema <- define "Close paren" (KeywordSchema "(")
  parentheses      <- define "Parentheses" (CompositeSchema [openParenSchema, expression, closeParenSchema])
  tuple            <- define "Tuple" (CompositeSchema [openParenSchema, tupleElements, closeParenSchema])
  tupleElements    <- define "Tuple elements" (ListSchema expression ",")
  variable         <- define "Variable" (IdentifierSchema "Variable")
  constructor      <- define "Constructor" (IdentifierSchema "Constructor")
  let'             <- define "Let-expression" (CompositeSchema [letKeyword, definitions, inKeyword, expression])
  letKeyword       <- define "Let keyword" (KeywordSchema "let")
  inKeyword        <- define "In keyword" (KeywordSchema "in")
  eqKeyword        <- define "= keyword" (KeywordSchema "=")
  definitions      <- define "Definitions" (ListSchema definition ";")
  definition       <- define "Definition" (CompositeSchema [variable, eqKeyword, expression])
  pure expression)
