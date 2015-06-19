{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Mellon.Controller
         ( Cmd
         , Controller
         , ControllerF(..)
         , lock
         , unlock
         , scheduleLock
         , unscheduleLock
         , runCmd
         ) where

import Control.Monad.Free (liftF, Free, MonadFree)
import Control.Monad.Free.TH (makeFreeCon)
import Data.Time.Clock (UTCTime(..))

-- | The pure controller's visible state. Note that any bookkeeping
-- needed to implement the controller's state (e.g., scheduling future
-- locks) is specific to the implementation.
data ControllerState
  = Locked
  | Unlocked UTCTime
  deriving (Eq)

-- | The pure controller eDSL.
data ControllerF next where
  Lock :: (ControllerState -> next) -> ControllerF next
  Unlock :: UTCTime -> (ControllerState -> next) -> ControllerF next
  ScheduleLock :: UTCTime -> next -> ControllerF next
  UnscheduleLock :: next -> ControllerF next

-- | This Functor instance cannot yet be derived automatically by GHC.
instance Functor ControllerF where
  fmap f (Lock g) = Lock (f . g)
  fmap f (Unlock d g)  = Unlock d (f . g)
  fmap f (ScheduleLock d x) = ScheduleLock d (f x)
  fmap f (UnscheduleLock x) = UnscheduleLock (f x)

type Controller = Free ControllerF

makeFreeCon 'Lock
makeFreeCon 'Unlock
makeFreeCon 'ScheduleLock
makeFreeCon 'UnscheduleLock

-- | The pure state machine commands.
data Cmd
  = LockCmd
  | UnlockCmd UTCTime
  deriving (Eq)

-- | The pure state machine interpreter.
runCmd :: Cmd -> ControllerState -> Controller (ControllerState)
runCmd LockCmd Locked = lock
runCmd LockCmd (Unlocked _) =
  do unscheduleLock
     lock
runCmd (UnlockCmd untilDate) Locked =
  do scheduleLock untilDate
     unlock untilDate
runCmd (UnlockCmd untilDate) (Unlocked scheduledDate) =
  if untilDate > scheduledDate
     then
       do scheduleLock untilDate
          unlock untilDate
     else unlock scheduledDate
