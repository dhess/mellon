module System.Mellon.Controller.NewConcurrent
         ( runConcurrentControllerT
         ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad.Trans.Free (iterM, iterT)
import Control.Monad.IO.Class
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, picosecondsToDiffTime)
import System.Mellon.Controller.NewController (ControllerF(..), ControllerT)
import System.Mellon.NewStateMachine (Cmd(..), State(..), StateMachineF(..), stateMachine)

runConcurrentControllerT :: MonadIO m => ControllerT m a -> m a
runConcurrentControllerT block =
  do m <- liftIO newEmptyMVar
     liftIO $ forkStateMachine m
     iterT (run m) block
  where run :: MonadIO m => MVar Cmd -> ControllerF (m a) -> m a
        run m (LockNow next) =
          do liftIO $ putMVar m LockNowCmd
             next
        run m (UnlockUntil untilDate next) =
          do liftIO $ putMVar m (UnlockCmd untilDate)
             next

forkStateMachine :: MVar Cmd -> IO ()
forkStateMachine m =
  do _ <- forkIO (iterM runSM (stateMachine Locked))
     return ()
  where runSM :: MonadIO m => StateMachineF (m a) -> m a
        runSM (Lock next) =
          do liftIO $ putStrLn "Lock"
             next
        runSM (ScheduleLock atDate next) =
          do _ <- liftIO $ forkIO (threadSleepUntil atDate >> lockAt atDate)
             next
        runSM (Unlock next) =
          do liftIO $ putStrLn "Unlock"
             next
        -- For this particular implementation, it's safe simply to
        -- ignore this command. When the "unscheduled" lock fires, the
        -- state machine will simply ignore it.
        runSM (UnscheduleLock next) = next
        runSM (WaitForCmd next) =
          do cmd <- liftIO $ takeMVar m
             next cmd

        lockAt :: UTCTime -> IO ()
        lockAt t = putMVar m (LockCmd t)

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
               | r <= 0                       = return ()
               | r > maxThreadDelayInDiffTime = threadDelay maxThreadDelay >> threadSleepUntil t
               | otherwise                    = threadDelay $ nominalDiffTimeToMicroseconds r

             maxThreadDelay :: Int
             maxThreadDelay = maxBound

             maxThreadDelayInDiffTime :: NominalDiffTime
             maxThreadDelayInDiffTime = diffTimeToNominalDiffTime $ picosecondsToDiffTime $ toInteger maxThreadDelay * 1000000
               where diffTimeToNominalDiffTime = realToFrac

             nominalDiffTimeToMicroseconds :: NominalDiffTime -> Int
             nominalDiffTimeToMicroseconds d = truncate $ d * 1000000
