{-# LANGUAGE OverloadedStrings #-}

module System.Mellon.Impl.ThreadedController
         ( ThreadedController
         , initThreadedController
         , lock
         , unlock
         ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Free (iterM)
import Control.Monad.IO.Class
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import qualified Data.Text as T (concat)
import qualified Data.Text.IO as T (putStrLn)
import qualified System.Mellon.Lock as Lock (Lock(..))
import System.Mellon.Controller (Cmd(..), Controller, ControllerF(..), State(..), runStateMachine)

default (Text)

-- | 'ThreadedController' combines a 'Lock' with a thread-based scheduling and
-- concurrency mechanism.
data ThreadedController =
  ThreadedController (MVar Cmd)

-- | Create a new 'ThreadedController'. This launches a new thread.
-- Communication is achived with the controller via its 'MVar'.
initThreadedController :: Lock.Lock l => l -> IO ThreadedController
initThreadedController l = do
  m <- newEmptyMVar
  _ <- forkIO (threadedController m l Locked)
  return (ThreadedController m)

-- | Send commands to a 'ThreadedController'.
lock :: ThreadedController -> IO ()
lock (ThreadedController m) = putMVar m LockNowCmd

unlock :: ThreadedController -> UTCTime -> IO ()
unlock (ThreadedController m) t = putMVar m (UnlockCmd t)

-- | Note: don't expose this to the user of the controller. It's only
-- used for scheduled locks in response to unlock commands.
lockAt :: ThreadedController -> UTCTime -> IO ()
lockAt (ThreadedController m) t = putMVar m (LockCmd t)

threadedController :: Lock.Lock l => MVar Cmd -> l -> State -> IO ()
threadedController m l = loop
  where loop state =
          do cmd <- takeMVar m
             newState <- runTC l (runStateMachine cmd state)
             loop newState

runTC :: (MonadIO m, Lock.Lock l) => l -> Controller a -> m a
runTC l = iterM runCmd
  where runCmd :: MonadIO m => ControllerF (m a) -> m a

        runCmd (Lock next) =
          do liftIO $ Lock.lock l
             next

        runCmd (Unlock next) =
          do liftIO $ Lock.unlock l
             next

        runCmd (ScheduleLock atDate next) =
          do liftIO $ T.putStrLn $ T.concat ["Lock at ", pack $ show atDate]
             next

        -- | For this particular implentation, it's safe to simply
        -- ignore this command. (When the "unscheduled" lock fires,
        -- the state machine will simply ignore it.)
        runCmd (UnscheduleLock next) = next
