-- | A controller that can be controlled simultaneously from multiple
-- threads.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mellon.ConcurrentController
         ( ConcurrentController
         , ConcurrentControllerT(..)
         , concurrentController
         , runConcurrentControllerT
         ) where

import Control.Applicative (Alternative)
import Control.Concurrent (MVar, forkIO, newMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Monad.Trans.Free (iterT)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, picosecondsToDiffTime)
import Mellon.MonadController.Class
import Mellon.LockDevice
import Mellon.StateMachine (Cmd(..), StateMachineF(..), execCmdT)

-- | Wraps a mutex around a 'LockDevice' so that it can be
-- manipulated atomically and concurrently from multiple threads.
--
-- Note that the constructor is not exported. Use
-- 'concurrentController' to create a new instance.
data ConcurrentController = forall l. LockDevice l =>
  ConcurrentController (MVar State) l

-- | Create a new 'ConcurrentController' by wrapping a 'LockDevice'.
--
-- Because it is thread-safe and guarantees atomicity, this controller
-- can be passed around to multiple threads and used simultaneously
-- from any of them.
--
-- Note: in order to synchronize the state machine and the lock, this
-- function will lock the device before returning the new controller
-- instance.
--
-- Note: the controller assumes that the lock device is only managed
-- by this controller. Do not use the lock device with another
-- controller instance.
concurrentController :: (LockDevice l) => l -> IO ConcurrentController
concurrentController l =
  do lockDevice l
     m <- newMVar Locked
     return $ ConcurrentController m l

-- | A monad transformer which adds a 'ConcurrentController' monad to an
-- existing monad. Because the monad transformer is an instance of
-- 'MonadController', you can use the 'MonadController' interface from
-- the new monad to manipulate the controller.
newtype ConcurrentControllerT m a =
  ConcurrentControllerT (ReaderT ConcurrentController m a)
  deriving (Alternative,Applicative,Functor,Monad,MonadTrans,MonadIO,MonadFix,MonadPlus)

-- | Run an action inside the 'ConcurrentControllerT' transformer
-- using the supplied 'ConcurrentController' and return the result.
runConcurrentControllerT :: (MonadIO m) => ConcurrentController -> ConcurrentControllerT m a -> m a
runConcurrentControllerT c (ConcurrentControllerT action) = runReaderT action c

instance (MonadIO m) => MonadController (ConcurrentControllerT m) where
  lockNow = runInMutex LockNowCmd
  unlockUntil = runInMutex . UnlockCmd
  state =
    do (ConcurrentController c _) <- ConcurrentControllerT ask
       liftIO $ readMVar c

runInMutex :: (MonadIO m) => Cmd -> ConcurrentControllerT m State
runInMutex cmd =
  do (ConcurrentController c _) <- ConcurrentControllerT ask
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
  do (ConcurrentController _ l) <- ConcurrentControllerT ask
     liftIO $ lockDevice l
     next
runSM (ScheduleLock atDate next) =
  do scheduleLockAt atDate
     next
runSM (RunUnlock next) =
  do (ConcurrentController _ l) <- ConcurrentControllerT ask
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
