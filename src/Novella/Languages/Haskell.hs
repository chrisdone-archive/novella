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
import Novella.Types (Schema(..), LexerName(..))
import Text.Read

-- | Grammar for Haskell.
grammar :: Grammar
grammar = $(checkGrammar $ runDefine $ mdo
  -- General expression
  expression       <- rule "Expression" (ChoiceSchema [variable, constructor, parentheses
                                                      ,tuple, let', application, string])
  application      <- rule "Application" (CompositeSchema [expression, expression])
  parentheses      <- rule "Parentheses" (CompositeSchema [openParenSchema, expression, closeParenSchema])
  -- Tuple
  tuple            <- rule "Tuple" (CompositeSchema [openParenSchema, tupleElements, closeParenSchema])
  tupleElements    <- rule "Tuple elements" (ListSchema expression ",")
  -- Identifiers
  variable         <- rule "Variable" (IdentifierSchema "Variable")
  constructor      <- rule "Constructor" (IdentifierSchema "Constructor")
  -- Let
  let'             <- rule "Let-expression" (CompositeSchema [letKeyword, openCurly, definitions, closeCurly, inKeyword, expression])
  definitions      <- rule "Definitions" (ListSchema definition ";")
  definition       <- rule "Definition" (CompositeSchema [variable, eqKeyword, expression])
  -- Literals
  string           <- rule "String" (CompositeSchema [doubleQuote, stringContents, doubleQuote])
  stringContents   <- rule "String contents" (TokenSchema (LexerName 'stringLexer))
  -- Keywords
  doubleQuote      <- rule "Double quote" (KeywordSchema "\"")
  openCurly        <- rule "Open curly" (KeywordSchema "{")
  closeCurly       <- rule "Close curly" (KeywordSchema "}")
  openParenSchema  <- rule "Open paren" (KeywordSchema "(")
  closeParenSchema <- rule "Close paren" (KeywordSchema ")")
  letKeyword       <- rule "Let keyword" (KeywordSchema "let")
  inKeyword        <- rule "In keyword" (KeywordSchema "in")
  eqKeyword        <- rule "= keyword" (KeywordSchema "=")
  pure expression)

-- | Lexing a Haskell string.
stringLexer :: String -> Either String String
stringLexer inp =
  case readMaybe ("\"" <> inp <> "\"") of
    Nothing -> Left "Could not read string contents."
    Just str -> Right str
