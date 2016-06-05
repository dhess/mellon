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

A controller provides two commands to the user: /lock/ and /unlock/.
User lock commands are effective immediately, and the device remains
locked until the user runs a subsequent unlock command.

User unlock commands are effective immediately, but also take a
'UTCTime' argument which specifies the date at which the controller
will automatically lock the device again, if it hasn't subsequently
been overridden by the user via an unlock command with a later
expiration date.

The controller uses the @mellon-core@ state machine via the
'transition' function. The output of that function determines both the
controller's next state, and also what actions, if any, the controller
should take in response to a state transition.

-}

module Mellon.Controller
       ( -- * The mellon-core controller
         Controller
       , controller
       , lockController
       , unlockController
       , queryController
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

-- | A concurrent, thread-safe controller type.
--
-- The controller is parameterized on its device type.
--
-- Note that the type's constructor is not exported. You must use the
-- 'controller' constructor to create a new instance of this type; it
-- ensures that the controller is initialized properly.
data Controller d =
  Controller {_state :: MVar State
             ,_device :: Device d}

-- | Create a new instance of a 'Controller', parameterized on the
-- device type 'Device' @d@.
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
-- to control the device while this controller instance is live.
--
-- The controller treats the device as a critical section; only one
-- thread at a time will issue operations to the device.
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

-- | Lock the device controlled by the controller immediately.
--
-- Returns the new state of the controller.
lockController :: (MonadIO m) => Controller d -> m State
lockController = runMachine InputLockNow

-- | Unlock the device controlled by the controller immediately, and
-- keep it unlocked until the specified 'UTCTime'.
--
-- If the specified time is in the past, then the device will remain
-- unlocked until it is locked via an explicit 'lockController'
-- action, or until this action is called again with a 'UTCTime' in
-- the future.
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
