{-|
Module      : Mellon.Controller.Async
Description : An asynchronous @mellon-core@ controller
Copyright   : (c) 2017, Quixoftic, LLC
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

This module implements a thread-safe, asynchronous controller.
Scheduled locks are run as background threads, which sleep until their
events fire.

== Exception safety

All the controller actions provided in this module are exception-safe.
If an exception occurs in a controller action (e.g., because the
device throws an exception), the controller will be restored to its
state as it was immediately prior to the execution of the action, and
the exception will be re-thrown. After handling the exception, you can
continue to execute actions on the controller, if you wish. However,
the controller and the device may be out of sync at that point, or the
device may continue to throw exceptions until it can be reset.

The safest action to take after an exception occurs in a controller is
to reset the device to a known working state; and then to create, from
scratch, a new controller for the device.

-}

module Mellon.Controller.Async
       ( -- * An asynchronous controller implementation
         Controller
       , controller
       , minUnlockTime
       , lockController
       , unlockController
       , queryController

         -- * Re-exported types
       , Device(..)
       , State(..)
       ) where

import Control.Concurrent
       (MVar, modifyMVar, newMVar, readMVar, threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time
       (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime,
        picosecondsToDiffTime)

import Mellon.Device (Device(..))
import Mellon.StateMachine
       (Input(..), Output(..), State(..), transition)

-- | A concurrent, thread-safe controller type parameterized on its
-- device type.
--
-- Note that the type's constructor is not exported. You must use the
-- 'controller' constructor to create a new value of this type; it
-- ensures that the controller is initialized properly.
data Controller d =
  Controller {_state :: !(MVar State)
             ,_minUnlockTime :: !NominalDiffTime
             ,_device :: !(Device d)}

-- | Create a new 'Controller' value to control the given 'Device'.
--
-- Controllers created by this constructor are thread-safe and may be
-- passed around and controlled simultaneously on multiple threads.
-- All actions exported by this module which act on a 'Controller'
-- value are thread-safe.
--
-- The controller locks and unlocks the given device in response to
-- user commands and expiring unlocks. The controller assumes that
-- this device has already been initialized and is ready for
-- operation. It also assumes that it exclusively owns the device; do
-- not pass the device to any other controllers or otherwise attempt
-- to control the device while the returned 'Controller' value is
-- live.
--
-- The controller treats the device as a critical section; only one
-- thread at a time will issue operations to the device.
--
-- In order to synchronize the current device state with the state
-- machine, the constructor will lock the device and set the state
-- machine's initial state to 'StateLocked' before returning the new
-- 'Controller' value.
--
-- The optional 'NominalDiffTime' argument can be used to prevent the
-- device from too rapidly switching from the locked->unlocked->locked
-- states (/glitching/). Effectively, it specifies the minimum amount
-- of time that the controller will unlock the device. This is useful
-- for handling delayed unlock commands (for example, if the user is
-- communicating with the controller via a network connection but the
-- unlock command is delayed in transit because connection is down or
-- lagged), extremely short unlock durations that might damage the
-- physical access device, or hacking attempts. When the controller
-- receives an unlock command, it compares the current time to the
-- unlock command's expiration date. If the difference between the two
-- times is less than the minimum unlock duration, or if the
-- expiration date is in the past, then the controller will
-- effectively ignore the unlock request. If the value of this
-- argument is 'Nothing' or is negative, the controller treats it as a
-- 0 value.
controller :: (MonadIO m) => Maybe NominalDiffTime -> Device d -> m (Controller d)
controller minUnlock device = liftIO $
  do lockDevice device
     mvar <- newMVar StateLocked
     return $ Controller mvar (maybe 0 (max 0) minUnlock) device

-- | Get the controller's minimum unlock time.
minUnlockTime :: Controller d -> NominalDiffTime
minUnlockTime = _minUnlockTime

-- | Immediately lock the device controlled by the controller.
--
-- Returns the new state of the controller.
lockController :: (MonadIO m) => Controller d -> m State
lockController = runMachine InputLockNow

-- | Immediately unlock the device controlled by the controller, and
-- keep it unlocked until the specified 'UTCTime'.
--
-- If the specified time is in the past, then the device will unlock
-- briefly, and then lock again after a brief amount of time.
-- (__NOTE__: this behavior is considered to be a bug and will be
-- fixed in a subsequent release.)
--
-- Returns the new state of the controller.
unlockController :: (MonadIO m) => UTCTime -> Controller d -> m State
unlockController date = runMachine (InputUnlock date)

-- | Query the controller's current state.
queryController :: (MonadIO m) => Controller d -> m State
queryController c = liftIO $ readMVar (_state c)

runMachine :: (MonadIO m) => Input -> Controller d -> m State
runMachine i c =
  let state = _state c
  in liftIO $
       modifyMVar state $ \currentState ->
         do nextState <- go $ transition currentState i
            return (nextState, nextState)
  where
    go :: (MonadIO m) => (Maybe Output, State) -> m State
    go (Nothing, s) = return s
    go (Just OutputLock, s) =
      do liftIO $ lockDevice (_device c)
         return s
    go (Just (OutputUnlock date), s) = liftIO $
      -- Don't let the lock glitch. If the expiration date is too near
      -- (or in the past), we ignore it (in which case we must keep
      -- the state machine in sync by telling it that the unlock has
      -- already expired).
      do now <- getCurrentTime
         if _minUnlockTime c `addUTCTime` now > date
           then return $ snd $ transition s (InputUnlockExpired date)
           else
             do unlockDevice (_device c)
                scheduleLock date
                return s
    go (Just (OutputRescheduleLock date), s) = liftIO $
      -- The device is already unlocked, so we don't need to worry
      -- about a glitch here.
      do scheduleLock date
         return s
    -- For this particular implementation, it's safe simply to ignore
    -- this command. When the "unscheduled" lock fires, the state
    -- machine will simply ignore it.
    go (Just OutputCancelLock, s) = return s

    scheduleLock :: UTCTime -> IO ()
    scheduleLock date =
      do a <- async $
                do threadSleepUntil date
                   void $ runMachine (InputUnlockExpired date) c
         -- Ensure exceptions which occur in the child thread are
         -- reported in the parent.
         link a

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
