-- | The Novella structured editor.

module Novella
  ( commandParser
  , commandConduit
  , transformState
  ) where

import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.State.Strict (StateT)
import           Data.Conduit
import           Data.Reparsec (ParserT)
import qualified Data.Reparsec.Sequence as Reparsec
import           Data.Sequence (Seq)
import           Novella.Types

-- | Produce a stream of commands from a stream of inputs.
commandConduit ::
     Monad m
  => ConduitT Input (Either CommandParseError Command) (ReaderT State m) ()
commandConduit = Reparsec.parseConduit commandParser

-- | Produce a command from a stream of inputs.
commandParser ::
     Monad m => ParserT (Seq Input) CommandParseError (ReaderT State m) Command
commandParser = undefined

-- | Transform the state given the config and the command.
transformState :: Monad m => Config -> Command -> StateT State m ()
transformState = undefined
