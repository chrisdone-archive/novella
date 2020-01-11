{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- |

module Novella.Types.Optics where

import Novella.Types
import Optics

--------------------------------------------------------------------------------
-- Deriving optics

makeLenses ''State
makeLenses ''TypedSlot
makePrisms ''Slot
makeLenses ''Query
makeLenses ''ListEditor
makeLenses ''CompositeEditor
makePrisms ''Cursor
makePrisms ''Node

--------------------------------------------------------------------------------
-- Manual optics

-- | Handy way to traverse the typed slot at the cursor in the state.
typedSlotTraversalAtCursor :: Traversal State State TypedSlot (Slot Node)
typedSlotTraversalAtCursor = traversalVL visit
  where
    visit f (State {_stateCursor, _stateTypedSlot, _stateLogFunc}) =
      State <$>
      fmap
        (flip (set typedSlotSlot) _stateTypedSlot)
        (traverseOf (typedSlotTraversalViaCursor _stateCursor) f _stateTypedSlot) <*>
      pure _stateCursor <*>
      pure _stateLogFunc

-- This traversal works on the typed slot at the cursor, if there is
-- one.
--
-- The cursor may be invalid.
--
-- We can use @failover'@ instead of @over@ to add a sanity check that
-- the traversal succeeded or not. Signalling an error if so ("The
-- cursor appears to be broken..."). A simple procedure like removing
-- inner layers of the cursor is a simple way to restore order in the
-- universe.
typedSlotTraversalViaCursor :: Cursor -> Traversal TypedSlot (Slot Node) TypedSlot (Slot Node)
typedSlotTraversalViaCursor =
  \case
    Here -> traversalVL id
    InList idx cursor ->
      typedSlotSlotNode %
      _FilledSlot %
      _ListNode %
      listEditorTypedSlot idx %>
      typedSlotTraversalViaCursor cursor
    InComposite idx cursor ->
      typedSlotSlotNode %
      _FilledSlot %
      _CompositeNode %
      compositeEditorTypedSlot idx %>
      typedSlotTraversalViaCursor cursor

-- | A list editor doesn't really need to store a [TypedSlot] when a
-- [Slot Node] will do; the schema never changes per item.
listEditorTypedSlot :: Int -> IxTraversal Int ListEditor ListEditor TypedSlot (Slot Node)
listEditorTypedSlot i = itraversalVL visit
  where
    visit f (ListEditor schema typedSlots delim) =
      ListEditor <$> pure schema <*>
      itraverse
        (\j slotNode ->
           if i == j
             then f j
                    (TypedSlot
                       {_typedSlotSchema = schema, _typedSlotSlot = slotNode})
             else pure slotNode)
        typedSlots <*>
      pure delim

-- | A composite editor doesn't really need to store a [TypedSlot] when a
-- [Slot Node] will do; the schema never changes per item.
compositeEditorTypedSlot :: Int -> IxTraversal Int CompositeEditor CompositeEditor TypedSlot (Slot Node)
compositeEditorTypedSlot i = itraversalVL visit
  where
    visit f (CompositeEditor typedSlots) =
      CompositeEditor <$>
      itraverse
        (\j typedSlot ->
           if i == j
             then fmap (flip (set typedSlotSlot) typedSlot) (f j typedSlot)
             else pure typedSlot)
        typedSlots

-- | A lens that receives schema typed slot, returning just the slot.
typedSlotSlotNode :: Lens TypedSlot (Slot Node) (Slot Node) (Slot Node)
typedSlotSlotNode = lens _typedSlotSlot (flip const)
