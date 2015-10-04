-- | A controller that can be controlled simultaneously from multiple
-- threads.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mellon.Controller.Concurrent
         ( ConcurrentControllerCtx
         , ConcurrentControllerT(..)
         , concurrentControllerCtx
         , runConcurrentControllerT
         ) where

import Control.Applicative (Alternative)
import Control.Concurrent (MVar, forkIO, newMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Monad.Trans.Free (iterT)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, picosecondsToDiffTime)
import Mellon.Controller.Monad.Class
import Mellon.Lock.Device
import Mellon.StateMachine (Cmd(..), StateMachineF(..), execCmdT)

-- | Wraps a mutex around a 'LockDevice' (i.e., creates a context) so
-- that it can be manipulated atomically and concurrently from
-- multiple threads.
--
-- Note that the constructor is not exported. Use
-- 'concurrentControllerCtx' to create a new instance.
data ConcurrentControllerCtx = forall l. LockDevice l =>
  ConcurrentControllerCtx (MVar State) l

-- | Create a new 'ConcurrentControllerCtx' by wrapping a 'LockDevice'.
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
concurrentControllerCtx :: (LockDevice l) => l -> IO ConcurrentControllerCtx
concurrentControllerCtx l =
  do lockDevice l
     m <- newMVar Locked
     return $ ConcurrentControllerCtx m l

-- | A monad transformer which adds a concurrent controller monad to an
-- existing monad. Because the monad transformer is an instance of
-- 'MonadController', you can use the 'MonadController' interface from
-- the new monad to manipulate the controller.
newtype ConcurrentControllerT m a =
  ConcurrentControllerT (ReaderT ConcurrentControllerCtx m a)
  deriving (Alternative,Applicative,Functor,Monad,MonadTrans,MonadIO,MonadFix,MonadPlus)

-- | Run an action inside the 'ConcurrentControllerT' transformer
-- using the supplied 'ConcurrentControllerCtx' and return the result.
runConcurrentControllerT :: (MonadIO m) => ConcurrentControllerCtx -> ConcurrentControllerT m a -> m a
runConcurrentControllerT c (ConcurrentControllerT action) = runReaderT action c

instance (MonadIO m) => MonadController (ConcurrentControllerT m) where
  lockNow = runInMutex LockNowCmd
  unlockUntil = runInMutex . UnlockCmd
  state =
    do (ConcurrentControllerCtx c _) <- ConcurrentControllerT ask
       liftIO $ readMVar c

runInMutex :: (MonadIO m) => Cmd -> ConcurrentControllerT m State
runInMutex cmd =
  do (ConcurrentControllerCtx c _) <- ConcurrentControllerT ask
     st <- liftIO $ takeMVar c
     newState <- iterT runSM (execCmdT cmd st)
     liftIO $ putMVar c $! newState
     return newState

lockAt :: (MonadIO m) => UTCTime -> ConcurrentControllerT m ()
lockAt date =
  do _ <- runInMutex (LockCmd date)
     return ()

scheduleLockAt :: (MonadIO m) => UTCTime -> ConcurrentControllerT m ()
scheduleLockAt date =
  do cc <- ConcurrentControllerT ask
     _ <- liftIO $ forkIO (threadSleepUntil date >> runConcurrentControllerT cc (lockAt date))
     return ()

-- 'ConcurrentControllerT's implementation of the 'StateMachineF' EDSL.
runSM :: (MonadIO m) => StateMachineF (ConcurrentControllerT m a) -> ConcurrentControllerT m a
runSM (RunLock next) =
  do (ConcurrentControllerCtx _ l) <- ConcurrentControllerT ask
     liftIO $ lockDevice l
     next
runSM (ScheduleLock atDate next) =
  do scheduleLockAt atDate
     next
runSM (RunUnlock next) =
  do (ConcurrentControllerCtx _ l) <- ConcurrentControllerT ask
     liftIO $ unlockDevice l
     next
-- For this particular implementation, it's safe simply to
-- ignore this command. When the "unscheduled" lock fires, the
-- state machine will simply ignore it.
runSM (UnscheduleLock next) = next

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
