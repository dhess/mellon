{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A @mellon@ controller expressed as a 'Control.Monad.Free.Free'
-- monad.

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

-- | The controller eDSL.
data ControllerF next where
  LockNow :: next -> ControllerF next
  UnlockUntil :: UTCTime -> next -> ControllerF next

instance Functor ControllerF where
  fmap f (LockNow x) = LockNow (f x)
  fmap f (UnlockUntil d x) = UnlockUntil d (f x)

-- | A controller monad transformer.
type ControllerT = FreeT ControllerF

-- | A basic controller. This controller is probably not very useful,
-- except possibly for testing.
type Controller = ControllerT Identity

-- | Lock the controller now.
makeFreeCon 'LockNow
-- | Unlock the controller until the given 'UTCTime'.
makeFreeCon 'UnlockUntil
