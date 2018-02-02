{-|
Module      : Mellon.StateMachine
Description : The @mellon-core@ state machine
Copyright   : (c) 2018, Quixoftic, LLC
License     : BSD3
Maintainer  : Drew Hess <dhess-src@quixoftic.com>
Stability   : experimental
Portability : non-portable

The @mellon-core@ state machine is the heart of the locking protocol.

A user of the @mellon-core@ package is not expected to interact
directly with the state machine, as the state machine is pure and is
not capable of setting timers or performing 'IO' actions on physical
access devices. In @mellon-core@, those operations are the
responsibility of controllers, and controllers are what users should
interact with; see the "Mellon.Controller" module. However,
understanding the state machine model is useful for understanding the
behavior of a @mellon-core@ application.

The state machine's behavior is quite simple:

* The locked state has indefinite duration.

* The unlocked state has an /expiration date/ (a 'UTCTime'). The
controller will inform the state machine when this date has passed
(since the state machine cannot keep time), at which point the state
machine will advise the controller to lock the device again.

* The user can (via the controller) send a lock command at any time,
which will immediately cancel any unlock currently in effect.

* If the user (via the controller) sends an unlock command while a
previous unlock command is still in effect, then the unlock with the
later expiration date "wins"; i.e., if the new expiration date is
later than the current one, the unlock period is effectively extended,
otherwise the device remains unlocked until the previously-specified
date.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}

module Mellon.StateMachine
       ( -- * The state machine types
         Input(..)
       , Output(..)
       , State(..)

         -- * The state machine implementation
       , transition
       ) where

import Data.Data
import Data.Time (UTCTime)
import GHC.Generics

{- $setup

>>> import Test.QuickCheck
>>> import Test.QuickCheck.Instances

-}

-- | The state machine's states.
data State
  = StateLocked
    -- ^ The state machine is in the locked state
  | StateUnlocked !UTCTime
    -- ^ The state machine is unlocked until the specified date.
  deriving (Eq,Show,Read,Generic,Data,Typeable)

-- | The state machine's inputs, i.e., commands sent to the machine by
-- a controller, either in response to a user's command, or in
-- response to an expired timer.
data Input
  = InputLockNow
    -- ^ Lock immediately, canceling any unlock currently in effect
  | InputUnlockExpired !UTCTime
    -- ^ An unlock command has expired. The unlock's expiration date
    -- is given by the specified 'UTCTime' timestamp. Note that in the
    -- @mellon-core@ protocol, these commands are only ever sent by
    -- the controller, which manages timed events, and never by the
    -- user directly.
  | InputUnlock !UTCTime
    -- ^ Unlock until the specified time. If no existing unlock
    -- command with a later expiration is currently in effect when
    -- this command is executed, the controller managing the state
    -- machine must schedule a lock to run at the specified time
    -- (i.e., when the unlock expires).
  deriving (Eq,Show,Read,Generic,Data,Typeable)

-- | The state machine's outputs, i.e., commands to be performed by a
-- controller.
--
-- It's reasonable to wonder why the 'OutputUnlock' and
-- 'OutputRescheduleLock' values take a 'UTCTime' parameter, when the
-- 'State' they're both always associated with ('StateUnlocked') also
-- takes a 'UTCTime' parameter. Indeed, their time values will always
-- be the same. However, this redundancy permits an interface to the
-- state machine where the state is implicit (e.g., in a state monad)
-- and the controller only "sees" the 'Output'.
data Output
  = OutputLock
    -- ^ Lock the device now
  | OutputUnlock !UTCTime
    -- ^ Unlock the device now and schedule a lock to run at the given
    -- time
  | OutputRescheduleLock !UTCTime
    -- ^ The date for the currently scheduled lock has changed.
    -- Reschedule it for the specified date. Note that the new date is
    -- guaranteed to be later than the previously-scheduled time.
  | OutputCancelLock
    -- ^ Cancel the currently scheduled lock and lock the device now
  deriving (Eq,Show,Read,Generic,Data,Typeable)

-- | Run one iteration of the state machine.
--
-- Note that some transitions require no action by the controller,
-- hence the first element of the returned pair (the 'Output' value)
-- is wrapped in 'Maybe'.
--
-- == Properties
--
-- prop> transition StateLocked InputLockNow == (Nothing,StateLocked)
-- prop> \date -> transition StateLocked (InputUnlockExpired date) == (Nothing,StateLocked)
-- prop> \date -> transition StateLocked (InputUnlock date) == (Just $ OutputUnlock date,StateUnlocked date)
-- prop> \date -> transition (StateUnlocked date) InputLockNow == (Just OutputCancelLock,StateLocked)
-- prop> \date -> transition (StateUnlocked date) (InputUnlockExpired date) == (Just OutputLock,StateLocked)
-- prop> \(date1, date2) -> date1 /= date2 ==> transition (StateUnlocked date1) (InputUnlockExpired date2) == (Nothing,StateUnlocked date1)
-- prop> \date -> transition (StateUnlocked date) (InputUnlock date) == (Nothing,StateUnlocked date)
-- prop> \(date1, date2) -> date2 > date1 ==> transition (StateUnlocked date1) (InputUnlock date2) == (Just $ OutputRescheduleLock date2,StateUnlocked date2)
-- prop> \(date1, date2) -> not (date2 > date1) ==> transition (StateUnlocked date1) (InputUnlock date2) == (Nothing,StateUnlocked date1)
transition :: State -> Input -> (Maybe Output, State)

-- Locked state transitions.
transition StateLocked InputLockNow            = (Nothing, StateLocked)
transition StateLocked (InputUnlockExpired _)  = (Nothing, StateLocked)
transition StateLocked (InputUnlock untilDate) =
  (Just $ OutputUnlock untilDate,StateUnlocked untilDate)

-- Unlocked state transitions.
transition (StateUnlocked _) InputLockNow =
  (Just OutputCancelLock, StateLocked)
transition (StateUnlocked scheduledDate) (InputUnlock untilDate) =
  if untilDate > scheduledDate
     then (Just $ OutputRescheduleLock untilDate, StateUnlocked untilDate)
     else (Nothing, StateUnlocked scheduledDate)
transition (StateUnlocked scheduledDate) (InputUnlockExpired lockDate) =
  -- In this case, the state machine is currently unlocked, and the
  -- controller is informing the state machine that a
  -- previously-scheduled lock event has fired; in other words, a
  -- previously-accepted unlock command has expired, and now it's time
  -- to lock the state machine again.
  --
  -- However, because of various race conditions between incoming
  -- asynchronous user commands and firing timer threads in the
  -- controller implementation, the state machine only act on the
  -- incoming lock command if its date matches the current state's
  -- "until date." If they match, then the incoming lock command
  -- "belongs" to the machine's current state, and the machine must
  -- heed it.
  --
  -- If the incoming lock command's date does /not/ match the current
  -- state's date, then there are 2 possible sub-cases:
  --
  -- 1. The lock command's date is /earlier/ than the current state's
  -- expiration date. This can only happen if the user sent a
  -- subsequent unlock command with a later expiration date, and the
  -- controller informed the state machine of the new request; but
  -- before the controller could cancel the earlier timer thread, that
  -- thread's timer expired and its lock command reached the state
  -- machine first.
  --
  -- 2. The lock command's date is /later/ than the current
  -- outstanding state's expiration date. This should probably never
  -- happen, but there might be an odd corner case that I'm not
  -- considering.
  --
  -- In case 1, the right thing to do is to ignore the lock command.
  -- The only question that remains is whether to treat case 2 as an
  -- error. The conservative thing to do would be to add an error
  -- state to the state machine, from which there is no recovery, but
  -- in the interest of keeping the controller robust in the face of
  -- errors, we simply ignore the lock command in this case, as well.
  if lockDate == scheduledDate
     then (Just OutputLock, StateLocked)
     else (Nothing, StateUnlocked scheduledDate)
