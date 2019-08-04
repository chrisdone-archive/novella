-- | All types for the VTY interface.

module Novella.Types where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

-- | A schema that nodes can implement.
data Schema
  = KeywordSchema !Keyword
    -- ^ Some keyword; constant, not editable.
  | TokenSchema !LexerName
    -- ^ Some literal token that can be lexed by the given lexer.
  | IdentifierSchema !IdentifierCategory
    -- ^ An identifier in the given category.
  | ChoiceSchema !(NonEmpty Schema)
    -- ^ A choice between the given schemas.
  | ListSchema !Schema !Delimiter
    -- ^ A list of the given schema, separated by delimiter.
  | CompositeSchema (NonEmpty Schema)
    -- ^ A composite of the given different schemas.

-- | State of the app.
data State

-- | Cursor within the tree.
data Cursor

-- | A text query for the right node.
data Query

-- | A slot in the tree.
data Slot a
  = EmptySlot
  | QuerySlot !Query
  | FilledSlot !a

-- | A slot with a type.
data TypedSlot =
  TypedSlot
    { typedSlotSchema :: !Schema
    , typedSlotSlot :: !(Slot Node)
    }

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

-- | Editor for a list.
data ListEditor =
  ListEditor
    { listEditorSchema :: !Schema
    , listEditorTypedSlots :: ![TypedSlot]
    , listEditorDelimiter :: !Delimiter
    }

-- | A list delimiter.
data Delimiter

-- | Editor for a tuple.
data TupleEditor =
  TupleEditor
    { tupleEditorTypedSlots :: !(NonEmpty TypedSlot)
    }

-- | Editor for a composite of editors.
data CompositeEditor =
  CompositeEditor
    { compositeEditorSchemas :: !(NonEmpty Schema)
    , compositeEditorMaybeSlot :: !(Maybe TypedSlot)
    }

-- | Editor for a identifier of editors.
data IdentifierEditor =
  IdentifierEditor
    { identifierEditorIdentifierCategory :: IdentifierCategory
    , identifierEditorSlot :: !(Slot Identifier)
    }

-- | Editor for an arbitrary string, with a lexer that can be
-- provided.
data TokenEditor =
  TokenEditor
    { tokenEditorLexerName :: !LexerName
    , tokenEditorMaybeToken :: !(Maybe Token)
    }

-- | Some lexed token.
newtype Token = Token String
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

-- | A ident looked up from a scope environment.
newtype Identifier =
  Identifier String
  deriving (Show, Eq, Ord)

-- | User input; key press, mouse click, etc.
data Input

-- | Events that cause a change in the state.
data Event
