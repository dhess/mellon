{-# LANGUAGE ScopedTypeVariables #-}

module System.Mellon.Impl.ThreadedController
         ( ThreadedController
         , initThreadedController
         , lock
         , quit
         , unlock
         ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad.Free (iterM)
import Control.Monad.IO.Class
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, picosecondsToDiffTime)
import qualified System.Mellon.Lock as Lock (Lock(..))
import System.Mellon.Controller (Cmd(..), Controller, ControllerF(..), State(..), runStateMachine)

-- | The controller's commands.
data ThreadedControllerCmd
  = ControllerCmd Cmd
  | Quit (MVar ())

-- | 'ThreadedController' combines a 'Lock' with a thread-based scheduling and
-- concurrency mechanism.
data ThreadedController =
  ThreadedController (MVar ThreadedControllerCmd)

-- | Create a new 'ThreadedController'. This launches a new thread.
-- Communication is achived with the controller via its 'MVar'.
initThreadedController :: Lock.Lock l => l -> IO ThreadedController
initThreadedController l = do
  Lock.lock l
  m <- newEmptyMVar
  _ <- forkIO (threadedController m l Locked)
  return (ThreadedController m)

-- | Send commands to a 'ThreadedController'.
lock :: ThreadedController -> IO ()
lock (ThreadedController m) = putMVar m (ControllerCmd LockNowCmd)

unlock :: ThreadedController -> UTCTime -> IO ()
unlock (ThreadedController m) t = putMVar m (ControllerCmd (UnlockCmd t))

-- | Blocks until the controller has actually quit.
quit :: ThreadedController -> IO ()
quit (ThreadedController m) =
  do s <- newEmptyMVar
     putMVar m (Quit s)
     takeMVar s

-- | Note: don't expose this to the user of the controller. It's only
-- used for scheduled locks in response to unlock commands.
lockAt :: MVar ThreadedControllerCmd -> UTCTime -> IO ()
lockAt m t = putMVar m (ControllerCmd (LockCmd t))

threadedController :: Lock.Lock l => MVar ThreadedControllerCmd -> l -> State -> IO ()
threadedController m l = loop
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

runTC :: (MonadIO m, Lock.Lock l) => MVar ThreadedControllerCmd  -> l -> Controller a -> m a
runTC mvar l = iterM runCmd
  where runCmd :: MonadIO m => ControllerF (m a) -> m a

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
