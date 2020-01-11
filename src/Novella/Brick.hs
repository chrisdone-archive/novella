{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Brick frontend for Novella.

module Novella.Brick where

import qualified Brick
import qualified Brick.Widgets.Border as Brick
import           Control.Monad.State.Strict (runState)
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.Foldable
import           Data.List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Reparsec
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Validation
import qualified Graphics.Vty as Vty
import           Novella
import           Novella.Types
import           RIO

--------------------------------------------------------------------------------
-- Brick-specific types

data BrickState = BrickState
  { state :: State
  , partial :: Maybe (Maybe (Seq Input) -> Reader State (Result (Reader State) (Seq Input) CommandParseError Command))
  }

data BrickMsg
  = BadModifierkeys (NonEmpty Vty.Modifier)
  | BadKey Vty.Key
  | ExitCommandGiven
  | KeyConsumeFailure CommandParseError
  | ParsedCommand Command
  | WaitingForMoreKeys
  deriving (Show)

--------------------------------------------------------------------------------
-- Brick app

app :: GLogFunc BrickMsg -> Config -> Brick.App BrickState () ()
app glogFunc config =
  Brick.App
    { appDraw = drawBrickState config
    , appChooseCursor = \_state [] -> Nothing
    , appHandleEvent =
        \state event -> runReaderT (handleEvent config state event) glogFunc
    , appStartEvent = pure
    , appAttrMap =
        const
          (Brick.attrMap
             Vty.defAttr
             [ (keywordAttr, Brick.fg Vty.green)
             , (selectedListItem, Brick.bg Vty.blue)
             ])
    }

--------------------------------------------------------------------------------
-- View

drawBrickState :: Config -> BrickState -> [Brick.Widget ()]
drawBrickState Config {configSchema = schema} BrickState {state} =
  case slot of
    QuerySlot q -> drawQuery schema schemaName q
    FilledSlot node -> drawNode node
  where
    TypedSlot {_typedSlotSchema = schemaName, _typedSlotSlot = slot} = typedSlot
    State {_stateTypedSlot = typedSlot} = state

drawNode :: Node -> [Brick.Widget ()]
drawNode =
  \case
    _ -> [Brick.str "TODO: drawNode"]

drawQuery :: Map SchemaName Schema -> SchemaName -> Query -> [Brick.Widget ()]
drawQuery schemas schemaName (Query string selection) =
  case M.lookup schemaName schemas of
    Nothing -> [Brick.str "INVALID SCHEMA NAME!"]
    Just schema ->
      [ Brick.vBox
          [ Brick.border (Brick.str string)
          , drawChoiceList selection (drawSchemaDeep schemas schema)
          ]
      ]

drawChoiceList :: Int -> [Brick.Widget n] -> Brick.Widget n
drawChoiceList selection xs =
  Brick.borderWithLabel
    (Brick.str "Choices")
    (Brick.vBox
       (zipWith
          (\i ->
             if i == selection
               then Brick.forceAttr selectedListItem
               else id)
          [0 ..]
          xs))

selectedListItem :: Brick.AttrName
selectedListItem = "selected-list-item"

keywordAttr :: Brick.AttrName
keywordAttr = "keyword"

drawSchemaDeep :: Map SchemaName Schema -> Schema -> [Brick.Widget n]
drawSchemaDeep schemas =
  \case
    KeywordSchema keyword -> [drawKeyword keyword]
    TokenSchema (LexerName name) -> [Brick.str ("<" ++ show name ++ ">")]
    IdentifierSchema (IdentifierCategory category) ->
      [Brick.str ("<" ++ category ++ ">")]
    ChoiceSchema schemaNames ->
      fmap (drawSchemaShallow schemas) (toList schemaNames)
    ListSchema schemaName (Delimiter _delimiter) ->
      [brickUnwords [Brick.str "LIST OF", drawSchemaShallow schemas schemaName]]
    CompositeSchema schemaNames ->
      map (drawSchemaShallow schemas) (toList schemaNames)

drawSchemaShallow :: Map SchemaName Schema -> SchemaName -> Brick.Widget n
drawSchemaShallow schemas schemaName =
  case M.lookup schemaName schemas of
    Nothing -> error "drawSchemaShallow"
    Just schema ->
      case schema of
        KeywordSchema keyword -> drawKeyword keyword
        TokenSchema (LexerName name) -> Brick.str ("<" ++ show name ++ ">")
        IdentifierSchema (IdentifierCategory category) ->
          Brick.str ("<" ++ category ++ ">")
        ChoiceSchema{} -> drawSchemaName schemaName
        ListSchema {} ->
          brickUnwords [Brick.str "LIST OF",drawSchemaAtomic schemas schemaName]
        CompositeSchema schemaNames ->
          brickUnwords (map (drawSchemaAtomic schemas) (toList schemaNames))

drawSchemaAtomic :: Map SchemaName Schema -> SchemaName -> Brick.Widget n
drawSchemaAtomic schemas schemaName =
  case M.lookup schemaName schemas of
    Nothing -> error "drawSchemaAtomic"
    Just schema ->
      case schema of
        KeywordSchema keyword -> drawKeyword keyword
        TokenSchema (LexerName name) -> Brick.str ("<" ++ show name ++ ">")
        IdentifierSchema (IdentifierCategory category) ->
          Brick.str ("<" ++ category ++ ">")
        ChoiceSchema{} -> drawSchemaName schemaName
        ListSchema {} -> drawSchemaName schemaName
        CompositeSchema {} -> drawSchemaName schemaName

drawKeyword :: Keyword -> Brick.Widget n
drawKeyword (Keyword string) = Brick.forceAttr keywordAttr (Brick.str string)

drawSchemaName :: SchemaName -> Brick.Widget n
drawSchemaName (SchemaName string) = Brick.str ("<" ++ string ++ ">")

brickUnwords :: [Brick.Widget n] -> Brick.Widget n
brickUnwords xs = Brick.hBox (intersperse (Brick.str " ") xs)

--------------------------------------------------------------------------------
-- Handling inputs

-- | Handle an incoming event.
handleEvent ::
     Config
  -> BrickState
  -> Brick.BrickEvent () ()
  -> ReaderT (GLogFunc BrickMsg) (Brick.EventM ()) (Brick.Next BrickState)
handleEvent config brickState event = do
  case event of
    Brick.VtyEvent (Vty.EvKey key modifiers) ->
      handleEvKey config brickState key modifiers
    _ -> lift (Brick.continue brickState)

-- | Handle incoming keys.
handleEvKey ::
     Config
  -> BrickState
  -> Vty.Key
  -> [Vty.Modifier]
  -> ReaderT (GLogFunc BrickMsg) (Brick.EventM ()) (Brick.Next BrickState)
handleEvKey config brickState@BrickState {state, partial} key modifiers =
  case traverse modifierToInput (Seq.fromList modifiers) of
    Failure badModifiers -> do
      glog (BadModifierkeys badModifiers)
      lift (Brick.continue brickState)
    Success modifierInputs ->
      case keyToInput key of
        Left badKey -> do
          glog (BadKey badKey)
          lift (Brick.continue brickState)
        Right keyInput ->
          case runReader
                 (fromMaybe
                    (parseResultT commandParser)
                    partial
                    (pure (modifierInputs <> pure keyInput)))
                 state of
            Done _inputsConsumed _position _more command -> do
              glog (ParsedCommand command)
              case runState (transformState config command) state of
                (ExitLoop, state') -> do
                  glog ExitCommandGiven
                  lift (Brick.halt (brickState {state = state'}))
                (ContinueLoop, state') ->
                  lift (Brick.continue (brickState {state = state'}))
            Failed _inputsConsumed _position _more reason -> do
              glog (KeyConsumeFailure reason)
              lift (Brick.continue brickState {partial = Nothing})
            Partial cont -> do
              glog WaitingForMoreKeys
              lift (Brick.continue brickState {partial = pure cont})

-- | Convert a Vty modifier to a regular Novella input.
modifierToInput :: Vty.Modifier -> Validation (NonEmpty Vty.Modifier) Input
modifierToInput =
  \case
    Vty.MShift -> pure ShiftInput
    Vty.MCtrl -> pure CtrlInput
    Vty.MAlt -> pure AltInput
    key -> Failure (pure key)

-- | Convert a Vty key to a regular Novella input.
keyToInput :: Vty.Key -> Either Vty.Key Input
keyToInput =
  \case
    Vty.KEsc -> pure (EscInput)
    Vty.KChar char -> pure (CharInput char)
    Vty.KBS -> pure (BackspaceInput)
    Vty.KEnter -> pure (EnterInput)
    Vty.KLeft -> pure (LeftInput)
    Vty.KRight -> pure (RightInput)
    Vty.KUp -> pure (UpInput)
    Vty.KDown -> pure (DownInput)
    key -> Left key
