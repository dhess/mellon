{-|
Module      : Mellon.StateMachine
Description : The @mellon-core@ state machine
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

The @mellon-core@ state machine is the heart of the locking protocol.
A physical access device under the control of @mellon-core@ has 2
possible states: locked; or unlocked until a given time, which we call
its /expiration date/. When an unlock expires, the @mellon-core@
controller will automatically lock it again.

The @mellon-core@ state machine model is shown in the following
flow chart:

<<https://s3-us-west-2.amazonaws.com/mellon/mellon-state-diagram.svg mellon-core state diagram>>

Note that the state machine model implemented in this module is
slightly different than the one shown in the diagram. Specificaly,
this implementation has two types of lock commands, whereas in the
diagram there is only one. This distinction is due to the fact that, in
a concurrent implementation of a controller, it may not be possible to
guarantee that the unlock-expiration-followed-by-lock sequence as
specified by the model is atomic: the controller could receive a new
unlock command from the user while it is arranging for the
previously-scheduled lock to fire. In this case, the controller needs
to distinguish between a user's lock command and a timed lock command.

A user of the @mellon-core@ package is not expected to interact
directly with the state machine, as the state machine is pure and is
not capable of setting timers or performing 'IO' actions on physical
access devices. In @mellon-core@, those operations are the
responsibility of controllers, and controllers are what users should
interact with; see the "Mellon.Controller" module. However,
understanding the state machine model is useful for understanding the
behavior of a @mellon-core@ application.

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
  | StateUnlocked UTCTime
    -- ^ The state machine is unlocked until the specified time. If
    -- the time is in the past, then the machine is unlocked
    -- indefinitely.
  deriving (Eq,Show,Read,Generic,Data,Typeable)

-- | The state machine's inputs, i.e., commands sent to the machine by
-- a controller, either in response to a user's command, or in
-- response to an expired timer.
data Input
  = InputLockNow
    -- ^ Lock immediately, canceling any unlock currently in effect
  | InputLock UTCTime
    -- ^ Lock immediately, as the result of an expiring unlock. The
    -- unlock's expiration date is given by the specified 'UTCTime'
    -- timestamp. Note that in the @mellon-core@ protocol, these
    -- commands are only ever sent by the controller in response to an
    -- expired unlock command, and never by the user directly.
  | InputUnlock UTCTime
    -- ^ Unlock until the specified time. If no existing unlock
    -- command with a later expiration is currently in effect when
    -- this command is executed, the controller managing the state
    -- machine must schedule a lock to run at the specified time
    -- (i.e., when the unlock expires).
  deriving (Eq,Show,Read,Generic,Data,Typeable)

-- | The state machine's outputs, i.e., commands to be performed by a
-- controller.
--
-- It's reasonable to wonder why the 'OutputUnlock' value takes a
-- 'UTCTime' parameter, when the 'State' it's always associated with
-- ('StateUnlocked') also takes a 'UTCTime' parameter. Indeed, their
-- time values will always be the same. However, this redundancy
-- permits an interface to the state machine where the state is
-- implicit (e.g., in a state monad) and the controller only "sees"
-- the 'Output'.
data Output
  = OutputLock
    -- ^ Lock the device now
  | OutputUnlock UTCTime
    -- ^ Unlock the device now and schedule a lock to run at the given
    -- time
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
-- prop> \date -> transition StateLocked (InputLock date) == (Nothing,StateLocked)
-- prop> \date -> transition StateLocked (InputUnlock date) == (Just $ OutputUnlock date,StateUnlocked date)
-- prop> \date -> transition (StateUnlocked date) InputLockNow == (Just OutputCancelLock,StateLocked)
-- prop> \date -> transition (StateUnlocked date) (InputLock date) == (Just OutputLock,StateLocked)
-- prop> \(date1, date2) -> date1 /= date2 ==> transition (StateUnlocked date1) (InputLock date2) == (Nothing,StateUnlocked date1)
-- prop> \date -> transition (StateUnlocked date) (InputUnlock date) == (Nothing,StateUnlocked date)
-- prop> \(date1, date2) -> date2 > date1 ==> transition (StateUnlocked date1) (InputUnlock date2) == (Just $ OutputUnlock date2,StateUnlocked date2)
-- prop> \(date1, date2) -> not (date2 > date1) ==> transition (StateUnlocked date1) (InputUnlock date2) == (Nothing,StateUnlocked date1)
transition :: State -> Input -> (Maybe Output, State)

-- Locked state transitions.
transition StateLocked InputLockNow            = (Nothing, StateLocked)
transition StateLocked (InputLock _)           = (Nothing, StateLocked)
transition StateLocked (InputUnlock untilDate) =
  (Just $ OutputUnlock untilDate,StateUnlocked untilDate)

-- Unlocked state transitions.
transition (StateUnlocked _) InputLockNow      = (Just OutputCancelLock, StateLocked)
transition (StateUnlocked scheduledDate) (InputUnlock untilDate) =
  if untilDate > scheduledDate
     then (Just $ OutputUnlock untilDate, StateUnlocked untilDate)
     else (Nothing, StateUnlocked scheduledDate)
transition (StateUnlocked scheduledDate) (InputLock lockDate) =
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
