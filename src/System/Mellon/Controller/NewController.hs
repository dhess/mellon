{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 'Controller' is an abstract typeclass whose instances combine a
-- 'System.Mellon.Lock.Lock' instance with a scheduler implementation,
-- providing the user-facing interface to the @mellon@ state machine
-- model. The user sends 'lock' and 'unlock' commands to a
-- 'Controller' instance, and the 'Controller' interacts with
-- 'System.Mellon.StateMachine.StateMachine' to execute the commands.

module System.Mellon.Controller.NewController
       ( Controller
       , ControllerF(..)
       , lockNow
       , unlockUntil
       ) where

import Control.Monad.Trans.Free (liftF, Free, MonadFree)
import Control.Monad.Free.TH (makeFreeCon)
import Data.Time (UTCTime)

data ControllerF next where
  LockNow :: next -> ControllerF next
  UnlockUntil :: UTCTime -> next -> ControllerF next

instance Functor ControllerF where
  fmap f (LockNow x) = LockNow (f x)
  fmap f (UnlockUntil d x) = UnlockUntil d (f x)

type Controller = Free ControllerF

makeFreeCon 'LockNow
makeFreeCon 'UnlockUntil
