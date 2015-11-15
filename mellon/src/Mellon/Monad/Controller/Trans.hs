-- | A controller that can be controlled simultaneously from multiple
-- threads.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mellon.Monad.Controller.Trans
         ( -- * Controller context
           ControllerCtx
         , controllerCtx
           -- * The Controller monad
         , Controller
         , ControllerT(..)
         , runController
         , runControllerT
         , lockNow
         , state
         , unlockUntil
         ) where

import Control.Applicative (Alternative)
import Control.Concurrent (MVar, ThreadId, newMVar, threadDelay)
import qualified Control.Concurrent as C (forkIO, putMVar, readMVar, takeMVar)
import Control.Monad.Trans.Free (iterT)
import Control.Monad.Reader
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, picosecondsToDiffTime)
import Mellon.Device.Class (Device)
import qualified Mellon.Device.Class as D (lockDevice)
import Mellon.Monad.Lock.Class (MonadLock(..))
import Mellon.Monad.Lock.Trans (LockT, runLockT)
import Mellon.Monad.StateMachine (Cmd(..), StateMachineF(..), State(..), transition)

-- | Wraps a mutex around a 'Device' (i.e., creates a context) so
-- that it can be manipulated atomically and concurrently from
-- multiple threads.
--
-- Note that the constructor is not exported. Use
-- 'controllerCtx' to create a new instance.
data ControllerCtx d = ControllerCtx { statemv :: MVar State, device :: d }

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
controllerCtx :: (Device d) => d -> IO (ControllerCtx d)
controllerCtx d =
  do D.lockDevice d
     m <- newMVar Locked
     return $ ControllerCtx m d

-- | A monad transformer which adds a controller monad to an
-- existing monad.
newtype ControllerT d m a =
  ControllerT { unControllerT :: ReaderT (ControllerCtx d) (LockT d m) a }
  deriving (Alternative,Applicative,Functor,Monad,MonadReader (ControllerCtx d), MonadLock,MonadIO,MonadFix,MonadPlus)

-- | Run an action inside the 'ControllerT' transformer
-- using the supplied 'ControllerCtx' and return the result.
runControllerT :: (MonadIO m, Device d) => ControllerCtx d -> ControllerT d m a -> m a
runControllerT c action = runLockT (device c) (runReaderT (unControllerT action) c)

-- | Lock the controller immediately.
lockNow :: (MonadIO m, Device d) => ControllerT d m State
lockNow = runMachine LockNowCmd

-- | Unlock the controller until the specified date.
unlockUntil :: (MonadIO m, Device d) => UTCTime -> ControllerT d m State
unlockUntil = runMachine . UnlockCmd

-- | Get the current conroller state.
state :: (MonadIO m) => ControllerT d m State
state = mvar >>= readMVar

-- | The simplest useful 'ControllerT' monad instance.
type Controller d = ControllerT d IO ()

-- | Run an action inside the 'Controller' monad using the supplied
-- 'ControllerCtx'.
runController :: (Device d) => ControllerCtx d -> Controller d -> IO ()
runController = runControllerT


-- Internal ControllerT actions.
--
readMVar :: (MonadIO m) => MVar a -> ControllerT d m a
readMVar = liftIO . C.readMVar

putMVar :: (MonadIO m) => MVar a -> a -> ControllerT d m ()
putMVar mv = liftIO . (C.putMVar mv)

takeMVar :: (MonadIO m) => MVar a -> ControllerT d m a
takeMVar = liftIO . C.takeMVar

forkIO :: (MonadIO m) => IO () -> m ThreadId
forkIO action = liftIO $ C.forkIO action

mvar :: (Monad m) => ControllerT d m (MVar State)
mvar = asks statemv

acquireState :: (MonadIO m) => ControllerT d m State
acquireState = mvar >>= takeMVar

releaseState :: (MonadIO m) => State -> ControllerT d m State
releaseState st =
  do mv <- mvar
     putMVar mv $! st
     return st

runMachine :: (MonadIO m, Device d) => Cmd -> ControllerT d m State
runMachine cmd =
  do currentState <- acquireState
     newState <- iterT runSM (transition cmd currentState)
     releaseState newState
  where
    runSM :: (MonadIO m, Device d) => StateMachineF (ControllerT d m a) -> ControllerT d m a
    runSM (RunLock next) = lockDevice >> next
    runSM (ScheduleLock atDate next) =
      do scheduleLockAt atDate
         next
    runSM (RunUnlock next) = unlockDevice >> next
    -- For this particular implementation, it's safe simply to
    -- ignore this command. When the "unscheduled" lock fires, the
    -- state machine will simply ignore it.
    runSM (UnscheduleLock next) = next

    scheduleLockAt :: (MonadIO m, Device d) => UTCTime -> ControllerT d m ()
    scheduleLockAt date =
      do cc <- ask
         _ <- forkIO (threadSleepUntil date >> runControllerT cc (lockAt date))
         return ()

    lockAt :: (MonadIO m, Device d) => UTCTime -> ControllerT d m ()
    lockAt date = runMachine (LockCmd date) >> return ()

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
