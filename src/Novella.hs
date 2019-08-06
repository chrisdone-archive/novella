module Novella where

import           Control.Monad.Trans.State.Strict (StateT)
import qualified Data.Reparsec as Reparsec
import qualified Data.Reparsec.Sequence as Reparsec
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Novella.Types

-- | Produce an event from an input. If no [valid] event is detected
eventParser :: Input -> Reparsec.ParserT (Seq Input) e m Event
eventParser = undefined

-- | Transform the state given the config and the event.
transformState :: Monad m => Config -> Event -> StateT State m ()
transformState config event = pure ()
