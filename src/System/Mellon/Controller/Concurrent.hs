-- | A concurrent @mellon@ controller implementation. When run, the
-- controller forks a separate unbound (i.e., 'forkIO') thread to
-- control the 'StateMachine', and communicates with it safely using
-- 'MVar's.
--
-- While running in the concurrent controller monad, the program can
-- execute controller commands without blocking.

module System.Mellon.Controller.Concurrent
         ( ConcurrentController
         , concurrentController
         , runConcurrentControllerT
         , runConcurrentStateMachine
         ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad.Trans.Free (iterT)
import Control.Monad.IO.Class
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, picosecondsToDiffTime)
import System.Mellon.Controller.Free (ControllerF(..), ControllerT)
import System.Mellon.StateMachine (Cmd(..), State(..), StateMachineF(..), execCmdT)
import System.Mellon.Lock

data CCCmd a
  = Quit a
  | SMCmd Cmd
  deriving (Eq)

-- | A 'ConcurrentController' uses 'MVar's and other Concurrent
-- Haskell mechanisms to run a controller/state machine in a
-- background thread(s).
--
-- Note that the constructor is not exported. Use
-- 'concurrentController' to create a new instance.
data ConcurrentController a =
  ConcurrentController {_cmd :: MVar (CCCmd a)
                       ,_quit :: MVar a}

-- | Make a new 'ConcurrentController' by creating the MVar you'll use
-- to communicate with it.
concurrentController :: IO (ConcurrentController a)
concurrentController =
  do cm <- newEmptyMVar
     qm <- newEmptyMVar
     return ConcurrentController {_cmd = cm, _quit = qm}

-- | Run a computation in the 'ControllerT' monad transformer.
--
-- Because it uses 'forkIO' and 'MVar', the wrapped monad must be an
-- instance of 'MonadIO'.
runConcurrentControllerT :: (MonadIO m) => ConcurrentController a -> ControllerT m a -> m a
runConcurrentControllerT (ConcurrentController cm qm) block =
  do result <- iterT run block
     liftIO $ putMVar cm $ Quit result
     _ <- liftIO $ takeMVar qm
     return result
  where run :: (MonadIO m) => ControllerF (m a) -> m a
        run (LockNow next) =
          do liftIO $ putMVar cm $ SMCmd LockNowCmd
             next
        run (UnlockUntil untilDate next) =
          do liftIO $ putMVar cm $ SMCmd (UnlockCmd untilDate)
             next

runConcurrentStateMachine :: (MonadIO m, MonadLock m) => ConcurrentController a -> m a
runConcurrentStateMachine (ConcurrentController cm qm) = loop Locked
  where loop state =
          do cmd <- liftIO $ takeMVar cm
             case cmd of
               (Quit result) ->
                 do liftIO $ putMVar qm result
                    return result
               (SMCmd smc) ->
                 do newState <- iterT runSM (execCmdT smc state)
                    loop newState

        runSM :: (MonadIO m, MonadLock m) => StateMachineF (m a) -> m a
        runSM (LockDevice next) =
          do lock ()
             next
        runSM (ScheduleLock atDate next) =
          do _ <- liftIO $ forkIO (threadSleepUntil atDate >> lockAt atDate)
             next
        runSM (UnlockDevice next) =
          do unlock ()
             next
        -- For this particular implementation, it's safe simply to
        -- ignore this command. When the "unscheduled" lock fires, the
        -- state machine will simply ignore it.
        runSM (UnscheduleLock next) = next

        lockAt :: UTCTime -> IO ()
        lockAt t = putMVar cm $ SMCmd (LockCmd t)

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
