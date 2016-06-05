{-|
Module      : Mellon.Controller
Description : The @mellon-core@ concurrent controller
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

module Mellon.Controller
       ( ControllerEnv
       , controllerEnv
       , controllerLock
       , controllerUnlock
       , controllerState
       ) where

import Control.Concurrent (MVar, ThreadId, threadDelay)
import qualified Control.Concurrent as C (forkIO, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, picosecondsToDiffTime)

import Mellon.Device (Device(..))
import Mellon.StateMachine (Input(..), Output(..), State(..), transition)

data ControllerEnv d =
  ControllerEnv {_state :: MVar State
                ,_device :: Device d}

controllerEnv :: (MonadIO m) => Device d -> m (ControllerEnv d)
controllerEnv device =
  do liftIO $ _lockDevice device
     mvar <- liftIO $ newMVar StateLocked
     return $ ControllerEnv mvar device

controllerLock :: (MonadIO m) => ControllerEnv d -> m State
controllerLock = runMachine InputLockNow

controllerUnlock :: (MonadIO m) => UTCTime -> ControllerEnv d -> m State
controllerUnlock date = runMachine (InputUnlock date)

controllerState :: (MonadIO m) => ControllerEnv d -> m State
controllerState c = readMVar (_state c)

runMachine :: (MonadIO m) => Input -> ControllerEnv d -> m State
runMachine i c =
  let state = _state c
  in do currentState <- takeMVar state
        let (cmd, newState) = transition currentState i
        go cmd
        putMVar state $! newState
        return newState
  where
    go :: (MonadIO m) => Maybe Output -> m ()
    go Nothing = return ()
    go (Just OutputLock) = liftIO $ _lockDevice (_device c)
    go (Just (OutputUnlock date)) =
      do liftIO $ _unlockDevice (_device c)
         void $
           forkIO $
             do threadSleepUntil date
                void $ liftIO $ runMachine (InputLock date) c
    -- For this particular implementation, it's safe simply to
    -- ignore this command. When the "unscheduled" lock fires, the
    -- state machine will simply ignore it.
    go (Just OutputCancelLock) = return ()

-- 'threadDelay' takes an 'Int' argument which is measured in
-- microseconds, so on 32-bit platforms, 'threadDelay' might not be
-- able to delay long enough to accommodate even a day's sleep.
-- Therefore, we need this mess.
--
-- Does not account for leap seconds and is only precise to about 1
-- second, but I think that's probably OK.
threadSleepUntil :: UTCTime -> IO ()
threadSleepUntil t =
  do now <- getCurrentTime
     let timeRemaining = diffUTCTime t now
     sleep timeRemaining
  where sleep :: NominalDiffTime -> IO ()
        sleep r
          | r <= 0 = return ()
          | r > maxThreadDelayInDiffTime = threadDelay maxThreadDelay >>
                                            threadSleepUntil t
          | otherwise = threadDelay $ nominalDiffTimeToMicroseconds r
        maxThreadDelay :: Int
        maxThreadDelay = maxBound
        maxThreadDelayInDiffTime :: NominalDiffTime
        maxThreadDelayInDiffTime = diffTimeToNominalDiffTime $ picosecondsToDiffTime $
                                                               toInteger maxThreadDelay *
                                                               1000000
          where diffTimeToNominalDiffTime = realToFrac
        nominalDiffTimeToMicroseconds :: NominalDiffTime -> Int
        nominalDiffTimeToMicroseconds d = truncate $ d * 1000000

-- MonadIO wrappers.
--
newMVar :: (MonadIO m) => a -> m (MVar a)
newMVar = liftIO . C.newMVar

readMVar :: (MonadIO m) => MVar a -> m a
readMVar = liftIO . C.readMVar

putMVar :: (MonadIO m) => MVar a -> a -> m ()
putMVar mv = liftIO . C.putMVar mv

takeMVar :: (MonadIO m) => MVar a -> m a
takeMVar = liftIO . C.takeMVar

forkIO :: (MonadIO m) => IO () -> m ThreadId
forkIO action = liftIO $ C.forkIO action
