{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Mellon.Controller.Concurrent
         ( ConcurrentController
         , ConcurrentControllerT(..)
         , concurrentController
         , runConcurrentControllerT
         ) where

import Control.Applicative (Alternative)
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad.Trans.Free (iterT)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, picosecondsToDiffTime)
import System.Mellon.Controller.Class
import System.Mellon.StateMachine (Cmd(..), State(..), StateMachineF(..), execCmdT)
import System.Mellon.Lock

-- | A 'ConcurrentController' uses 'MVar's and other Concurrent
-- Haskell mechanisms to run a controller/state machine in a
-- background thread(s).
--
-- Note that the constructor is not exported. Use
-- 'concurrentController' to create a new instance.
data ConcurrentController =
  ConcurrentController (MVar State)
  deriving (Eq)

-- | Create a new 'ConcurrentController'.
concurrentController :: State -> IO ConcurrentController
concurrentController initialState =
  do m <- newEmptyMVar
     putMVar m initialState
     return $ ConcurrentController m

newtype ConcurrentControllerT m a =
  ConcurrentControllerT (ReaderT ConcurrentController m a)
  deriving (Alternative,Applicative,Functor,Monad,MonadTrans,MonadIO,MonadFix,MonadPlus)

instance (MonadIO m, MonadLock m) => MonadController (ConcurrentControllerT m) where
  lockNow =
    do (ConcurrentController c) <- ConcurrentControllerT ask
       state <- liftIO $ takeMVar c
       lock
       liftIO $ putMVar c state
  unlockUntil _ =
    do (ConcurrentController c) <- ConcurrentControllerT ask
       state <- liftIO $ takeMVar c
       unlock
       liftIO $ putMVar c state

instance (MonadLock m) => MonadLock (ConcurrentControllerT m) where
  lock = lift lock
  unlock = lift unlock

runConcurrentControllerT :: (MonadIO m) => ConcurrentControllerT m a -> ConcurrentController -> m a
runConcurrentControllerT (ConcurrentControllerT action) c = runReaderT action c

-- runConcurrentStateMachine :: (MonadIO m, MonadLock m) => ConcurrentController a -> m a
-- runConcurrentStateMachine (ConcurrentController m) = loop Locked
--   where loop state =
--           do cmd <- liftIO $ takeMVar cm
--              case cmd of
--                (Quit result) ->
--                  do liftIO $ putMVar qm result
--                     return result
--                (SMCmd smc) ->
--                  do newState <- iterT runSM (execCmdT smc state)
--                     loop newState

--         runSM :: (MonadIO m, MonadLock m) => StateMachineF (m a) -> m a
--         runSM (LockDevice next) =
--           do lock
--              next
--         runSM (ScheduleLock atDate next) =
--           do _ <- liftIO $ forkIO (threadSleepUntil atDate >> lockAt atDate)
--              next
--         runSM (UnlockDevice next) =
--           do unlock
--              next
--         -- For this particular implementation, it's safe simply to
--         -- ignore this command. When the "unscheduled" lock fires, the
--         -- state machine will simply ignore it.
--         runSM (UnscheduleLock next) = next

--         lockAt :: UTCTime -> IO ()
--         lockAt t = putMVar cm $ SMCmd (LockCmd t)

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
