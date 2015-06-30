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
import System.Mellon.Impl.MockLock (MockLock, initMockLock)
import qualified System.Mellon.Lock as Lock (Lock(..))
import System.Mellon.Controller (Cmd(..), Controller, ControllerF(..), State(..), runStateMachine)

default (Text)

-- | 'MockController' combines a 'MockLock' with a simple scheduling and
-- concurrency mechanism.
data MockController =
  MockController (MVar Cmd)

-- | Internal bookkeeping.
data MockControllerState =
  MockControllerState (MVar Cmd)
                      MockLock
                      State

-- | Create a new 'MockController'.
initMockController :: IO MockController
initMockController = do
  m <- newEmptyMVar
  l <- initMockLock
  let c = MockControllerState m l Locked
  _ <- forkIO (mockController c)
  return (MockController m)

-- | Send commands to a 'MockController'.
lockMockController :: MockController -> IO ()
lockMockController (MockController m) = putMVar m LockCmd

unlockMockController :: MockController -> UTCTime -> IO ()
unlockMockController (MockController m) t = putMVar m (UnlockCmd t)

mockController :: MockControllerState -> IO ()
mockController (MockControllerState m mockLock initialState) = loop initialState
  where loop state =
          do cmd <- takeMVar m
             newState <- runMockControllerCmd mockLock (runStateMachine cmd state)
             loop newState

runMockControllerCmd :: MonadIO m => MockLock -> Controller a -> m a
runMockControllerCmd l = iterM runMC
  where runMC :: MonadIO m => ControllerF (m a) -> m a

        runMC (Lock next) =
          do liftIO $ Lock.lock l
             next

        runMC (Unlock next) =
          do liftIO $ Lock.unlock l
             next

        runMC (ScheduleLock atDate next) =
          do liftIO $ T.putStrLn $ T.concat ["Lock at ", pack $ show atDate]
             next

        runMC (UnscheduleLock next) =
          do liftIO $ T.putStrLn "Unschedule lock"
             next

