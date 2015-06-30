{-# LANGUAGE OverloadedStrings #-}

module System.Mellon.Impl.MockController
         ( MockController
         , initMockController
         , lockMockController
         , unlockMockController
         ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Free (iterM)
import Control.Monad.IO.Class
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import qualified Data.Text as T (concat)
import qualified Data.Text.IO as T (putStrLn)
import System.Mellon.Lock (Lock(..))
import System.Mellon.Controller (Cmd(..), Controller, ControllerF(..), State(..), runStateMachine)

default (Text)

-- | 'MockController' combines a 'Lock' with a simple scheduling and
-- concurrency mechanism.
data MockController =
  MockController (MVar Cmd)

-- | Create a new 'MockController'.
initMockController :: Lock l => l -> IO MockController
initMockController lck = do
  mvar <- newEmptyMVar
  _ <- forkIO (mockController mvar lck Locked)
  return (MockController mvar)

-- | Send commands to a 'MockController'.
lockMockController :: MockController -> IO ()
lockMockController (MockController m) = putMVar m LockCmd

unlockMockController :: MockController -> UTCTime -> IO ()
unlockMockController (MockController m) t = putMVar m (UnlockCmd t)

mockController :: Lock l => MVar Cmd -> l -> State -> IO ()
mockController mvar lck = loop
  where loop state =
          do cmd <- takeMVar mvar
             newState <- runMockControllerCmd lck (runStateMachine cmd state)
             loop newState

runMockControllerCmd :: (MonadIO m, Lock l) => l -> Controller a -> m a
runMockControllerCmd lck = iterM runMC
  where runMC :: MonadIO m => ControllerF (m a) -> m a

        runMC (Lock next) =
          do liftIO $ lock lck
             next

        runMC (Unlock next) =
          do liftIO $ unlock lck
             next

        runMC (ScheduleLock atDate next) =
          do liftIO $ T.putStrLn $ T.concat ["Lock at ", pack $ show atDate]
             next

        runMC (UnscheduleLock next) =
          do liftIO $ T.putStrLn "Unschedule lock"
             next

