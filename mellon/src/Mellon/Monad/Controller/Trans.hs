-- | A controller that can be controlled simultaneously from multiple
-- threads.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mellon.Monad.Controller.Trans
         ( ControllerCtx
         , Controller
         , ControllerT(..)
         , controllerCtx
         , lockNow
         , runController
         , runControllerT
         , state
         , unlockUntil
         ) where

import Control.Applicative (Alternative)
import Control.Concurrent (MVar, forkIO, newMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Monad.Trans.Free (iterT)
import Control.Monad.Reader
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, picosecondsToDiffTime)
import Mellon.Device.Class
import Mellon.Monad.StateMachine (Cmd(..), StateMachineF(..), State(..), transition)

-- | Wraps a mutex around a 'Device' (i.e., creates a context) so
-- that it can be manipulated atomically and concurrently from
-- multiple threads.
--
-- Note that the constructor is not exported. Use
-- 'controllerCtx' to create a new instance.
data ControllerCtx = forall d. Device d =>
  ControllerCtx (MVar State) d

-- | Create a new 'ControllerCtx' by wrapping a 'Device'.
--
-- Because it is thread-safe and guarantees atomicity, this controller
-- context can be passed around to multiple threads and used
-- simultaneously from any of them.
--
-- Note: in order to synchronize the state machine and the lock, this
-- function will lock the device before returning the new controller
-- instance.
--
-- Note: the controller context assumes that the lock device is only
-- managed by this controller context. Do not use the same lock device
-- with multiple controller context instances.
controllerCtx :: (Device d) => d -> IO ControllerCtx
controllerCtx d =
  do lockDevice d
     m <- newMVar Locked
     return $ ControllerCtx m d

-- | A monad transformer which adds a controller monad to an
-- existing monad. Because the monad transformer is an instance of
-- 'MonadController', you can use the 'MonadController' interface from
-- the new monad to manipulate the controller.
newtype ControllerT m a =
  ControllerT (ReaderT ControllerCtx m a)
  deriving (Alternative,Applicative,Functor,Monad,MonadTrans,MonadIO,MonadFix,MonadPlus)

mvar :: (Monad m) => ControllerT m (MVar State)
mvar =
  do (ControllerCtx c _) <- ctx
     return c

ctx :: (Monad m) => ControllerT m ControllerCtx
ctx = ControllerT ask

-- | Run an action inside the 'ControllerT' transformer
-- using the supplied 'ControllerCtx' and return the result.
runControllerT :: (MonadIO m) => ControllerCtx -> ControllerT m a -> m a
runControllerT c (ControllerT action) = runReaderT action c

-- | Lock the controller immediately.
lockNow :: (MonadIO m) => ControllerT m State
lockNow = runAtomically LockNowCmd

-- | Unlock the controller until the specified date.
unlockUntil :: (MonadIO m) => UTCTime -> ControllerT m State
unlockUntil = runAtomically . UnlockCmd

-- | Get the current conroller state.
state :: (MonadIO m) => ControllerT m State
state = mvar >>= liftIO . readMVar

lockAt :: (MonadIO m) => UTCTime -> ControllerT m ()
lockAt date = runAtomically (LockCmd date) >> return ()

runAtomically :: (MonadIO m) => Cmd -> ControllerT m State
runAtomically cmd =
  do mv <- mvar
     st <- liftIO $ takeMVar mv
     newState <- iterT runSM (transition cmd st)
     liftIO $ putMVar mv $! newState
     return newState

scheduleLockAt :: (MonadIO m) => UTCTime -> ControllerT m ()
scheduleLockAt date =
  do cc <- ctx
     _ <- liftIO $ forkIO (threadSleepUntil date >> runControllerT cc (lockAt date))
     return ()

-- 'ControllerT's implementation of the 'StateMachineF' EDSL.
runSM :: (MonadIO m) => StateMachineF (ControllerT m a) -> ControllerT m a
runSM (RunLock next) =
  do (ControllerCtx _ l) <- ctx
     liftIO $ lockDevice l
     next
runSM (ScheduleLock atDate next) =
  do scheduleLockAt atDate
     next
runSM (RunUnlock next) =
  do (ControllerCtx _ l) <- ctx
     liftIO $ unlockDevice l
     next
-- For this particular implementation, it's safe simply to
-- ignore this command. When the "unscheduled" lock fires, the
-- state machine will simply ignore it.
runSM (UnscheduleLock next) = next

-- | The simplest useful 'ControllerT' monad instance.
type Controller = ControllerT IO ()

-- | Run an action inside the 'Controller' monad.
runController :: ControllerCtx -> Controller -> IO ()
runController = runControllerT

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
