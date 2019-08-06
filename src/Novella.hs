-- | The Novella structured editor.

module Novella
  ( commandParser
  , commandConduit
  , transformState
  ) where

import           Control.Monad.Trans
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.State.Strict (StateT)
import           Data.Conduit
import           Data.Reparsec (ParserT)
import qualified Data.Reparsec as Reparsec
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Novella.Types

-- | Produce a stream of commands from a stream of inputs.
commandConduit ::
     Monad m
  => ConduitT Input (Either CommandParseError Command) (ReaderT State m) ()
commandConduit = go (Reparsec.parseResultT commandParser)
  where
    go parser = do
      v <- await
      result <- lift (parser (fmap pure v))
      case result of
        Reparsec.Done input pos _more command -> do
          mapM_ leftover (Seq.drop pos input)
          yield (Right command)
          commandConduit
        Reparsec.Failed remaining pos _more errors -> do
          mapM_ leftover (Seq.drop pos remaining)
          yield (Left errors)
          commandConduit
        Reparsec.Partial cont ->
          go cont

-- | Produce a command from a stream of inputs.
commandParser ::
     Monad m => ParserT (Seq Input) CommandParseError (ReaderT State m) Command
commandParser = undefined

-- | Transform the state given the config and the command.
transformState :: Monad m => Config -> Command -> StateT State m ()
transformState = undefined
