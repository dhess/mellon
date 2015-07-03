{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Mellon.StateMachine
         ( Cmd(..)
         , StateMachine
         , StateMachineF(..)
         , State(..)
         , runStateMachine
         ) where

import Control.Monad.Free (liftF, Free, MonadFree)
import Control.Monad.Free.TH (makeFreeCon)
import Data.Time (UTCTime)

-- | Generally a user process does not interact directly with a
-- 'StateMachine'. That is typically the job of a 'Controller', and
-- the user interacts with the 'Controller.' A 'StateMachine' is a
-- pure representation of the abstract Mellon state machine.
data State
  = Locked
  | Unlocked UTCTime
  deriving (Eq)

-- | The 'StateMachine' eDSL, used by a 'StateMachine' to control a
-- 'Controller'. Each 'Controller' implementation will provide an
-- implementation of the eDSL to turn the 'StateMachine's pure state
-- transformations into real-world actions.
data StateMachineF next where
  Lock :: next -> StateMachineF next
  ScheduleLock :: UTCTime -> next -> StateMachineF next
  Unlock :: next -> StateMachineF next
  UnscheduleLock :: next -> StateMachineF next

-- | This Functor instance cannot yet be derived automatically by GHC.
instance Functor StateMachineF where
  fmap f (Lock x) = Lock (f x)
  fmap f (ScheduleLock d x) = ScheduleLock d (f x)
  fmap f (Unlock x)  = Unlock (f x)
  fmap f (UnscheduleLock x) = UnscheduleLock (f x)

-- | 'StateMachine' expressed as a 'Free' monad.
type StateMachine = Free StateMachineF

makeFreeCon 'Lock
makeFreeCon 'Unlock
makeFreeCon 'ScheduleLock
makeFreeCon 'UnscheduleLock

-- | The pure 'StateMachine' commands. Commands are passed, along with
-- the current state, to 'runStateMachine' by a 'Controller' in order
-- to operate the Mellon state machine, which then calls back into the
-- 'Controller' via the 'StateMachineF' eDSL.
data Cmd
  = LockNowCmd
  | LockCmd UTCTime
  | UnlockCmd UTCTime
  deriving (Eq)

-- | The pure 'StateMachine' interpreter.
--
-- 'runStateMachine' provides an abstract, pure model of the core
-- Mellon state machine. The state machine is the same for all
-- implementations; what changes from one implementation to the next
-- is the specific machinery for locking and scheduling, which is
-- provided by a 'Controller' implementation.
runStateMachine :: Cmd -> State -> StateMachine State

runStateMachine LockNowCmd Locked = return Locked
runStateMachine LockNowCmd (Unlocked _) =
  do unscheduleLock
     lock
     return Locked

runStateMachine (LockCmd _) (Locked) = return Locked
runStateMachine (LockCmd lockDate) (Unlocked untilDate) =
  -- | Only execute the lock command if its date matches the current
  -- outstanding unlock request's expiration date, i.e., if the lock
  -- command is the one that was scheduled by the current outstanding
  -- unlock request.
  --
  -- If the lock command's date does not match the current outstanding
  -- lock request's date, there are 2 possible cases:
  --
  -- 1. The lock command's date is earlier than the current
  -- outstanding unlock's expiration date. This means that the lock
  -- command's corresponding unlock command was overridden by a
  -- subsequent unlock with a later expiration date before the lock
  -- command fired, hence the state machine should ignore this lock
  -- command.
  --
  -- 2. The lock command's date is later than the current outstanding
  -- unlock's expiration date. You might think this should never
  -- happen, and indeed for a controller implementation that does
  -- strict bookkeeping and actually bothers to "unschedule" scheduled
  -- locks when a "lock now" command is received, it is extremely
  -- unlikely to occur... but I believe that for certain threaded
  -- controller implementations, it probably could, theoretically.
  -- Regardless, there's no harm in simply ignoring the request, as
  -- whatever unlock command is currently in progress, eventually it
  -- will either be canceled, or its own scheduled lock command will
  -- fire, in which case the dates will match exactly and everything
  -- will behave as expected.
  --
  -- In either case (1 or 2), the right thing to do is to ignore the
  -- lock command. The only question that remains is whether to treat
  -- case 2 as an error. For some very strict implementations, it's
  -- possible that it could be a real error, but I suspect that for
  -- most implementations, it's a very unlikely but probably harmless
  -- occurrence. That's how we treat it here.
  if lockDate == untilDate
     then lock >> return Locked
     else return (Unlocked untilDate)

runStateMachine (UnlockCmd untilDate) Locked = unlockUntil untilDate
runStateMachine (UnlockCmd untilDate) (Unlocked scheduledDate) =
  if untilDate > scheduledDate
     then unlockUntil untilDate
     else return $ Unlocked scheduledDate

unlockUntil :: UTCTime -> StateMachine State
unlockUntil date =
  do scheduleLock date
     unlock
     return $ Unlocked date
