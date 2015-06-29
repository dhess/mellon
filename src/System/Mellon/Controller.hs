{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Mellon.Controller
         ( Cmd(..)
         , Controller
         , ControllerF(..)
         , ControllerState(..)
         , lock
         , unlock
         , scheduleLock
         , unscheduleLock
         , runCmd
         ) where

import Control.Monad.Free (liftF, Free, MonadFree)
import Control.Monad.Free.TH (makeFreeCon)
import Data.Time (UTCTime)

-- | 'Controller' visible state. Note that any bookkeeping
-- needed to implement the controller's state (e.g., scheduling future
-- locks) is specific to the concrete implementation.
data ControllerState
  = Locked
  | Unlocked UTCTime
  deriving (Eq)

-- | The 'Controller' eDSL.
data ControllerF next where
  Lock :: next -> ControllerF next
  Unlock :: next -> ControllerF next
  ScheduleLock :: UTCTime -> next -> ControllerF next
  UnscheduleLock :: next -> ControllerF next

-- | This Functor instance cannot yet be derived automatically by GHC.
instance Functor ControllerF where
  fmap f (Lock x) = Lock (f x)
  fmap f (Unlock x)  = Unlock (f x)
  fmap f (ScheduleLock d x) = ScheduleLock d (f x)
  fmap f (UnscheduleLock x) = UnscheduleLock (f x)

-- | 'Controller' expressed as a 'Free' monad.
type Controller = Free ControllerF

makeFreeCon 'Lock
makeFreeCon 'Unlock
makeFreeCon 'ScheduleLock
makeFreeCon 'UnscheduleLock

-- | The pure state machine commands. Commands are passed, along with
-- the current state, to 'runCmd' in order to operate the Mellon state
-- machine.
data Cmd
  = LockCmd
  | UnlockCmd UTCTime
  deriving (Eq)

-- | The pure state machine interpreter.
--
-- 'runCmd' provides an abstract, pure model of the core Mellon state
-- machine. The state machine is the same for all implementations;
-- what changes from one implementation to the next is the specific
-- machinery for locking and scheduling. Each kind of scheduler/lock
-- implementation provides its own implementation of the 'Controller'
-- eDSL language. 'runCmd' is parameterized on the 'Controller' 'Free'
-- monad, hence it works with any implementation of that monad.
runCmd :: Cmd -> ControllerState -> Controller ControllerState
runCmd LockCmd Locked =
  do lock
     return Locked
runCmd LockCmd (Unlocked _) =
  do unscheduleLock
     lock
     return Locked
runCmd (UnlockCmd untilDate) Locked = unlockUntil untilDate
runCmd (UnlockCmd untilDate) (Unlocked scheduledDate) =
  if untilDate > scheduledDate
     then unlockUntil untilDate
     else return $ Unlocked scheduledDate

unlockUntil :: UTCTime -> Controller ControllerState
unlockUntil untilDate =
  do scheduleLock untilDate
     unlock
     return $ Unlocked untilDate
