-- | In @mellon@, a 'ControllerT' is the interface between the user
-- program and thea @mellon@
-- 'System.Mellon.StateMachine.StateMachineT'. A useful controller (as
-- opposed to one used only for debugging or testing) will combine a
-- 'System.Mellon.Lock.MonadLock' implementation with an
-- implementation of the 'System.Mellon.StateMachine.StateMachineF'
-- EDSL to provide the user with a mechanism for locking an unlocking
-- a physical device according to the @mellon@ state machine protocol.
--

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Mellon.Controller.Free
       ( Controller
       , ControllerF(..)
       , ControllerT
       , lockNow
       , unlockUntil
       ) where

import Control.Monad.Trans.Free (liftF, FreeT, MonadFree)
import Control.Monad.Free.TH (makeFreeCon)
import Data.Functor.Identity (Identity)
import Data.Time (UTCTime)

-- | The (quite simple) 'ControllerT' embedded DSL.
data ControllerF next where
  LockNow :: next -> ControllerF next
  UnlockUntil :: UTCTime -> next -> ControllerF next

instance Functor ControllerF where
  fmap f (LockNow x) = LockNow (f x)
  fmap f (UnlockUntil d x) = UnlockUntil d (f x)

-- | 'ControllerT' is a free monad; controller implementations provide
-- the machinery for turning the 'ControllerF' EDSL into actions that
-- transition the @mellon@ 'System.Mellon.StateMachine.StateMachineT'
-- from one state to the next. A controller implementation is also
-- responsible for keeping track of the current state of the state
-- machine.
type ControllerT = FreeT ControllerF

-- | 'Controller' wraps 'ControllerT' around the 'Identity' monad.
-- This controller is probably not very useful, except possibly for
-- testing.
type Controller = ControllerT Identity

-- | Tell the controller to lock its device immediately. Any
-- previously-executed unlock command that is currently in effect will
-- be canceled.
makeFreeCon 'LockNow
-- | Tell the controller to unlock its device until a given 'UTCTime',
-- at which time the controller will automatically lock the device
-- again. Note that you can unlock the device indefinitely by
-- specifying a time in the past.
makeFreeCon 'UnlockUntil
