{-|
Module      : Mellon.Controller
Description : The @mellon-core@ controller
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

In @mellon-core@, controllers are the intermediary between the
@mellon-core@ state machine, the physical access device, and the user
who wants to control the device. The user interacts directly only with
the controller, not with the physical access device or the state
machine.

A controller takes two commands from the user: /lock/ and /unlock/.
User lock commands go into effect immediately. User unlock commands
take a 'UTCTime' argument which specifies the date at which the
controller will automatically lock the device again, if it hasn't been
overridden by a subsequent unlock command with a later expiration
date.

The controller uses the @mellon-core@ state machine to sequence
events, and to determine the next state after a user command or
expiring unlock. The controller uses the physical access device to
lock and unlock the device.

This module provides a concurrent, thread-safe controller
implementation.

-}

module Mellon.Controller
       ( ControllerEnv
       , controllerEnv
       , controllerLock
       , controllerUnlock
       , controllerState
       ) where

import Control.Concurrent
       (MVar, forkIO, newMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time
       (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime,
        picosecondsToDiffTime)

import Mellon.Device (Device(..))
import Mellon.StateMachine
       (Input(..), Output(..), State(..), transition)

data ControllerEnv d =
  ControllerEnv {_state :: MVar State
                ,_device :: Device d}

controllerEnv :: (MonadIO m) => Device d -> m (ControllerEnv d)
controllerEnv device = liftIO $
  do lockDevice device
     mvar <- newMVar StateLocked
     return $ ControllerEnv mvar device

controllerLock :: (MonadIO m) => ControllerEnv d -> m State
controllerLock = runMachine InputLockNow

controllerUnlock :: (MonadIO m) => UTCTime -> ControllerEnv d -> m State
controllerUnlock date = runMachine (InputUnlock date)

controllerState :: (MonadIO m) => ControllerEnv d -> m State
controllerState c = liftIO $ readMVar (_state c)

runMachine :: (MonadIO m) => Input -> ControllerEnv d -> m State
runMachine i c =
  let state = _state c
  in liftIO $
       do currentState <- takeMVar state
          let (cmd, newState) = transition currentState i
          go cmd
          putMVar state $! newState
          return newState
  where
    go :: (MonadIO m) => Maybe Output -> m ()
    go Nothing = return ()
    go (Just OutputLock) = liftIO $ lockDevice (_device c)
    go (Just (OutputUnlock date)) = liftIO $
      do unlockDevice (_device c)
         void $
           forkIO $
             do threadSleepUntil date
                void $ runMachine (InputLock date) c
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
