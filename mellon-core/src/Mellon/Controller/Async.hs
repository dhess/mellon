{-|
Module      : Mellon.Controller.Async
Description : An asynchronous @mellon-core@ controller
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

This module implements a thread-safe, asynchronous controller.
Scheduled locks are run as background threads, which sleep until their
events fire.

-}

module Mellon.Controller.Async
       ( -- * An asynchronous controller implementation
         Controller
       , controller
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
  Controller {_state :: MVar State
             ,_device :: Device d}

-- | Create a new 'Controller' value, parameterized on the device type
-- 'Device' @d@.
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
-- N.B. The controller makes no guarantees about the period between
-- invoking actions on the device. If your device requires a delay
-- bewtween successive operations, you should build that delay into
-- your 'Device' @d@ implementation.
controller :: (MonadIO m) => Device d -> m (Controller d)
controller device = liftIO $
  do lockDevice device
     mvar <- newMVar StateLocked
     return $ Controller mvar device

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

minSleep :: NominalDiffTime
minSleep = 1

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
      do now <- getCurrentTime
         if minSleep `addUTCTime` now > date
           then return $ snd $ transition s (InputLock date)
           else
             do unlockDevice (_device c)
                a <- async $
                       do threadSleepUntil date
                          void $ runMachine (InputLock date) c
                -- Ensure exceptions which occur in the child thread are
                -- reported in the parent.
                link a
                return s
    -- For this particular implementation, it's safe simply to
    -- ignore this command. When the "unscheduled" lock fires, the
    -- state machine will simply ignore it.
    go (Just OutputCancelLock, s) = return s

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
