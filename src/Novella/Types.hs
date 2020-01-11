{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}

-- | All types for the VTY interface.

module Novella.Types where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Reparsec as Reparsec
import           Data.Sequence (Seq)
import           Data.String
import           Instances.TH.Lift ()
import           Language.Haskell.TH.Lift (Lift)
import           Language.Haskell.TH.Syntax (Name)
import           RIO (HasGLogFunc(..), GLogFunc, gLogFuncL)

--------------------------------------------------------------------------------
-- Schemas describing the syntax of a language

-- | A statically-checked grammar.
data Grammar =
  Grammar
    { grammarToplevel :: !SchemaName
    , grammarRules :: !(Map SchemaName Schema)
    }
  deriving (Lift, Show)

-- | Used to name grammar productions, to avoid the confusion of
-- recursive grammars (no structural equality or show instances).
newtype SchemaName =
  SchemaName String
  deriving (Show, Eq, Ord, IsString, Lift)

-- | A schema that nodes can implement.
data Schema
  = KeywordSchema !Keyword
    -- ^ Some keyword; constant, not editable.
  | TokenSchema !LexerName
    -- ^ Some literal token that can be lexed by the given lexer.
  | IdentifierSchema !IdentifierCategory
    -- ^ An identifier in the given category.
  | ListSchema !SchemaName !Delimiter
    -- ^ A list of the given schema, separated by delimiter.
  | CompositeSchema !(NonEmpty SchemaName)
    -- ^ A composite of the given different schemas.
  | ChoiceSchema !(NonEmpty SchemaName)
    -- ^ A choice between the given schemas.
  deriving (Show, Eq, Lift)

newtype Delimiter =
  Delimiter String
  deriving (Show, Eq, Ord, IsString, Lift)

-- | Lexer name that can be used to find a lexer.
newtype LexerName =
  LexerName Name
  deriving (Show, Eq, Ord, Lift)

-- | A category of identifier.
newtype IdentifierCategory =
  IdentifierCategory String
  deriving (Show, Eq, Ord, IsString, Lift)

-- | A keyword rendered in the UX. Not editable.
newtype Keyword =
  Keyword String
  deriving (Show, Eq, Ord, IsString, Lift)

--------------------------------------------------------------------------------
-- High-level types for the UI

-- | Config of the app.
data Config =
  Config
    { configSchema :: !(Map SchemaName Schema)
    }
  deriving (Show)

-- | State of the app.
data State =
  State
    { _stateTypedSlot :: !TypedSlot
    , _stateCursor :: !Cursor
    , _stateLogFunc :: !(GLogFunc NovellaMsg)
    }
instance HasGLogFunc State where
  type GMsg State = NovellaMsg
  gLogFuncL f s = fmap (\lf -> s {_stateLogFunc = lf}) (f (_stateLogFunc s))

-- | A log message for novella generally.
data NovellaMsg =
  CommandParserMsg CommandParserMsg
  deriving (Show)

-- | A log message for a command parser.
data CommandParserMsg =
  ParsingForSlot TypedSlot
  | FoundSchema Schema
  deriving (Show)

-- | A flag to indicate what to do after transforming the state.
data Loop
  = ExitLoop
  | ContinueLoop

-- | Cursor pointing to a place within the tree. Tagged so that we can
-- go up/down easily.
data Cursor
  = Here
  | InList !Int !Cursor
  | InComposite !Int !Cursor
  deriving (Show, Eq)

-- | User input; key press, mouse click, etc.
data Input
  = CharInput !Char
  | CtrlInput
  | ShiftInput
  | AltInput
  | EscInput
  | BackspaceInput
  | DeleteInput
  | TabInput
  | SpaceInput
  | UpInput
  | DownInput
  | LeftInput
  | RightInput
  | EnterInput
  | MetaInput
  deriving (Show, Eq)

-- | Commands that cause a change in the state.
data Command
  = QuitCommand
  | EXITCommand
  | CtrlCCommand
  | UpdateQuery Query
  deriving (Show, Eq)

-- | A match of a query to a schema name.
data Match =
  Match SchemaName
  deriving (Show, Eq)

-- | An error resulting from trying to parse inputs into a command.
data CommandParseError
  = ExpectedButGot Input Input
  | NoMoreInput
  | ManyProblems (NonEmpty CommandParseError)
  | NoCurrentFocusedNode
  | NoNodeMatches
  | NoSuchSchemaToQuery SchemaName
  | NoActionForFilledSlot
  | QueryingKeywordSchema Keyword
  | UnknownCommand
  deriving (Show)

instance Semigroup CommandParseError where
  (<>) x y = ManyProblems (pure x <> pure y)

instance Reparsec.UnexpectedToken Input CommandParseError where
  expectedButGot = ExpectedButGot

instance Reparsec.NoMoreInput CommandParseError where
  noMoreInputError = NoMoreInput

--------------------------------------------------------------------------------
-- The UI tree of editors

-- | A slot with a type.
data TypedSlot =
  TypedSlot
    { _typedSlotSchema :: !SchemaName
    , _typedSlotSlot :: !(Slot Node)
    }
  deriving (Show, Eq)

-- | A slot in the tree.
data Slot a
  = QuerySlot !Query
  | FilledSlot !a
  deriving (Show, Eq, Functor, Traversable, Foldable)

-- | A node in the tree.
data Node
  = KeywordNode !Keyword
    -- ^ A non-editable keyword.
  | TokenNode !TokenEditor
    -- ^ Token node.
  | IdentifierNode !IdentifierEditor
    -- ^ A lookup node (variable, constructor, anything named).
  | ListNode !ListEditor
    -- ^ An arbitrary-sized list of a single schema type of slots.
  | CompositeNode !CompositeEditor
    -- ^ A fixed-size n-tuple of different schema type slots.
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Editors

-- | Editor for a identifier of editors.
data IdentifierEditor =
  IdentifierEditor
    { identifierEditorIdentifierCategory :: IdentifierCategory
    , identifierEditorSlot :: !Identifier
    }
  deriving (Show, Eq)

-- | Editor for an arbitrary string, with a lexer that can be
-- provided.
data TokenEditor =
  TokenEditor
    { tokenEditorLexerName :: !LexerName
    , tokenEditorMaybeToken :: !(Maybe Token)
    }
  deriving (Show, Eq)

-- | Editor for a list.
data ListEditor =
  ListEditor
    { _listEditorSchema :: !SchemaName
    , _listEditorSlots :: ![Slot Node]
    , _listEditorDelimiter :: !Delimiter
    }
  deriving (Show, Eq)

-- | Editor for a composite of editors.
data CompositeEditor =
  CompositeEditor
    { _compositeEditorTypedSlots :: !(NonEmpty TypedSlot)
    }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- UI auxilliary types

-- | Some lexed token.
newtype Token =
  Token String
  deriving (Show, Eq, Ord)

-- | A ident looked up from a scope environment.
newtype Identifier =
  Identifier String
  deriving (Show, Eq, Ord)

-- | A text query for the right node.
data Query = Query
  { _queryText :: String
  , _querySelection :: Int
  , _queryMatches :: Seq Match
  } deriving (Show, Eq)
