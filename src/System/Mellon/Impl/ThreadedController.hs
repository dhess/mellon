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
initThreadedController lck = do
  mvar <- newEmptyMVar
  _ <- forkIO (threadedController mvar lck Locked)
  return (ThreadedController mvar)

-- | Send commands to a 'ThreadedController'.
lock :: ThreadedController -> IO ()
lock (ThreadedController mvar) = putMVar mvar LockCmd

unlock :: ThreadedController -> UTCTime -> IO ()
unlock (ThreadedController mvar) t = putMVar mvar (UnlockCmd t)

threadedController :: Lock.Lock l => MVar Cmd -> l -> State -> IO ()
threadedController mvar lck = loop
  where loop state =
          do cmd <- takeMVar mvar
             newState <- runTC lck (runStateMachine cmd state)
             loop newState

runTC :: (MonadIO m, Lock.Lock l) => l -> Controller a -> m a
runTC lck = iterM runCmd
  where runCmd :: MonadIO m => ControllerF (m a) -> m a

        runCmd (Lock next) =
          do liftIO $ Lock.lock lck
             next

        runCmd (Unlock next) =
          do liftIO $ Lock.unlock lck
             next

        runCmd (ScheduleLock atDate next) =
          do liftIO $ T.putStrLn $ T.concat ["Lock at ", pack $ show atDate]
             next

        runCmd (UnscheduleLock next) =
          do liftIO $ T.putStrLn "Unschedule lock"
             next

