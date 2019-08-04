{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
-- | All types for the VTY interface.

module Novella.Types where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------
-- Schemas describing the syntax of a language

-- | Used to name grammar productions, to avoid the confusion of
-- recursive grammars (no structural equality or show instances).
newtype SchemaName =
  SchemaName String
  deriving (Show, Eq, Ord)

-- | A schema that nodes can implement.
data Schema
  = KeywordSchema !Keyword
    -- ^ Some keyword; constant, not editable.
  | TokenSchema !LexerName
    -- ^ Some literal token that can be lexed by the given lexer.
  | IdentifierSchema !IdentifierCategory
    -- ^ An identifier in the given category.
  | ChoiceSchema !(NonEmpty SchemaName)
    -- ^ A choice between the given schemas.
  | ListSchema !SchemaName !Delimiter
    -- ^ A list of the given schema, separated by delimiter.
  | CompositeSchema !(NonEmpty SchemaName)
    -- ^ A composite of the given different schemas.
  deriving (Show, Eq)

newtype Delimiter =
  Delimiter String
  deriving (Show, Eq, Ord)

-- | Lexer name that can be used to find a lexer.
newtype LexerName =
  LexerName String
  deriving (Show, Eq, Ord)

-- | A category of identifier.
newtype IdentifierCategory =
  IdentifierCategory String
  deriving (Show, Eq, Ord)

-- | A keyword rendered in the UX. Not editable.
newtype Keyword =
  Keyword String
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- High-level types for the UI

-- | State of the app.
data State =
  State
    { stateTypedSlot :: !TypedSlot
    , stateCursor :: !Cursor
    , stateSchema :: !(Map SchemaName Schema)
    }
  deriving (Show, Eq)

-- | Cursor pointing to a place within the tree.
data Cursor
  = InList !Int !Cursor
  | InComposite !Int !Cursor
  | InChoice !Cursor
  | Here
  deriving (Show, Eq)

-- | User input; key press, mouse click, etc.
data Input

-- | Events that cause a change in the state.
data Event

--------------------------------------------------------------------------------
-- The UI tree of editors

-- | A slot in the tree.
data Slot a
  = EmptySlot
  | QuerySlot !Query
  | FilledSlot !a
  deriving (Show, Eq, Functor, Traversable, Foldable)

-- | A slot with a type.
data TypedSlot =
  TypedSlot
    { typedSlotSchema :: !SchemaName
    , typedSlotSlot :: !(Slot Node)
    }
  deriving (Show, Eq)

-- | A node in the tree.
data Node
  = KeywordNode !Keyword
    -- ^ A non-editable keyword.
  | ListNode !ListEditor
    -- ^ An arbitrary-sized list of a single schema type of slots.
  | TupleNode !TupleEditor
    -- ^ A fixed-size n-tuple of different schema type slots.
  | CompositeNode !CompositeEditor
    -- ^ A composite of schemas and a single slot.
  | IdentifierNode !IdentifierEditor
    -- ^ A lookup node (variable, constructor, anything named).
  | TokenNode !TokenEditor
    -- ^ Token node.
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Editors

-- | Editor for a list.
data ListEditor =
  ListEditor
    { listEditorSchema :: !SchemaName
    , listEditorTypedSlots :: ![TypedSlot]
    , listEditorDelimiter :: !Delimiter
    }
  deriving (Show, Eq)

-- | Editor for a tuple.
data TupleEditor =
  TupleEditor
    { tupleEditorTypedSlots :: !(NonEmpty TypedSlot)
    }
  deriving (Show, Eq)

-- | Editor for a composite of editors.
data CompositeEditor =
  CompositeEditor
    { compositeEditorSchemas :: !(NonEmpty SchemaName)
    , compositeEditorMaybeSlot :: !(Maybe TypedSlot)
    }
  deriving (Show, Eq)

-- | Editor for a identifier of editors.
data IdentifierEditor =
  IdentifierEditor
    { identifierEditorIdentifierCategory :: IdentifierCategory
    , identifierEditorSlot :: !(Slot Identifier)
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
newtype Query =
  Query String
  deriving (Show, Eq, Ord)
