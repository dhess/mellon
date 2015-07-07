{-# LANGUAGE ScopedTypeVariables #-}

-- | 'ThreadedController' combines a 'Lock.Lock' with a thread-based
-- scheduling and concurrency mechanism.

module System.Mellon.Controller.ThreadedController
         ( initThreadedController
         ) where

import Control.Concurrent (MVar, forkIO, putMVar, takeMVar, threadDelay)
import Control.Monad.Free (iterM)
import Control.Monad.IO.Class
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, picosecondsToDiffTime)
import qualified System.Mellon.Lock as Lock (Lock(..))
import System.Mellon.Controller.Concurrent
import System.Mellon.StateMachine (StateMachine, StateMachineF(..), State(..), runStateMachine)

-- | Create a new 'ConcurrentController' using the given 'Lock.Lock'
-- instance. This will lock the 'Lock.Lock'.
initThreadedController :: Lock.Lock l => l -> IO ConcurrentController
initThreadedController l = do
  Lock.lock l
  forkCC (threadedController l)

threadedController :: Lock.Lock l => l -> MVar ConcurrentControllerCmd -> State -> IO ()
threadedController l m = loop
  where loop state =
          do cmd <- takeMVar m
             case cmd of
               Quit s ->
                 do Lock.quit l
                    putMVar s ()
               ControllerCmd cc ->
                 do newState <-
                      runTC m l (runStateMachine cc state)
                    loop newState

runTC :: (MonadIO m, Lock.Lock l) => MVar ConcurrentControllerCmd  -> l -> StateMachine a -> m a
runTC mvar l = iterM runCmd
  where runCmd :: MonadIO m => StateMachineF (m a) -> m a

        runCmd (Lock next) =
          do liftIO $ Lock.lock l
             next

        runCmd (Unlock next) =
          do liftIO $ Lock.unlock l
             next

        runCmd (ScheduleLock atDate next) =
          do _ <- liftIO $ forkIO (threadSleepUntil atDate >> lockAt mvar atDate)
             next

        -- | For this particular implentation, it's safe to simply
        -- ignore this command. (When the "unscheduled" lock fires,
        -- the state machine will simply ignore it.)
        runCmd (UnscheduleLock next) = next

-- | 'threadDelay' takes an 'Int' argument which is measured in
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
