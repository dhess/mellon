-- | 'TimedController' combines a 'Lock.Lock' with a periodic timer-based
-- scheduling system. A 'TimedController' wakes up approximately once
-- a second to check for incoming commands or expired unlocks. If
-- there's nothing to do, the controller goes back to sleep.
--
-- Due to its sleep/wake-based design, a 'TimedController' has a
-- resolution of about 1 second.
--
-- In most cases, you'll probably want to use a
-- 'System.Mellon.Conroller.ThreadedController.ThreadedController';
-- however, if for some reason you want to minimize the number of
-- threads created by the controller and aren't particularly
-- power-sensitive, use 'TimedController'.

module System.Mellon.Controller.TimedController
         ( initTimedController
         ) where

import Control.Concurrent (MVar, putMVar, threadDelay, tryTakeMVar)
import Control.Monad (when)
import Control.Monad.Free (iterM)
import Control.Monad.IO.Class
import Data.Time (addUTCTime, diffUTCTime, getCurrentTime)
import qualified System.Mellon.Lock as Lock (Lock(..))
import System.Mellon.Controller.Concurrent
import System.Mellon.StateMachine (StateMachine, StateMachineF(..), State(..), runStateMachine)

-- | Create a new 'ConcurrentController' using the given 'Lock.Lock' instance.
-- This will lock the 'Lock.Lock'.
initTimedController :: Lock.Lock l => l -> IO ConcurrentController
initTimedController l =
  do Lock.lock l
     forkCC (timedController l)

timedController :: Lock.Lock l => l -> MVar ConcurrentControllerCmd -> State -> IO ()
timedController l m = loop
  where loop state =
          do loopStartTime <- getCurrentTime
             maybeCmd <- tryTakeMVar m
             case maybeCmd of
               Just (Quit s) ->
                 do Lock.quit l
                    putMVar s ()
               Just (ControllerCmd cc) ->
                 do newState <-
                      runTC l (runStateMachine cc state)
                    sleepAndLoop loopStartTime newState
               Nothing ->
                  case state of
                    (Unlocked expiration) ->
                      if loopStartTime >= expiration
                         then lockAt m expiration >> loop state -- Don't sleep
                         else sleepAndLoop loopStartTime state
                    _ -> sleepAndLoop loopStartTime state

        sleepAndLoop startTime state =
          do now <- getCurrentTime
             let timeTilNextWake = 1 `addUTCTime` startTime `diffUTCTime` now
             when (timeTilNextWake > 0 ) $ threadDelay (truncate $ timeTilNextWake * 1e6)
             loop state

runTC :: (MonadIO m, Lock.Lock l) => l -> StateMachine a -> m a
runTC l = iterM runCmd
  where runCmd :: MonadIO m => StateMachineF (m a) -> m a

        runCmd (Lock next) =
          do liftIO $ Lock.lock l
             next

        runCmd (Unlock next) =
          do liftIO $ Lock.unlock l
             next

        -- | 'TimedController' doesn't need to schedule anything.
        runCmd (ScheduleLock _ next) = next
        runCmd (UnscheduleLock next) = next
