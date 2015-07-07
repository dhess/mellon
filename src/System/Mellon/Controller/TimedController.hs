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
         ( TimedController
         , initTimedController
         ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay, tryTakeMVar)
import Control.Monad (when)
import Control.Monad.Free (iterM)
import Control.Monad.IO.Class
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import qualified System.Mellon.Lock as Lock (Lock(..))
import System.Mellon.Controller.Controller (Controller(..))
import System.Mellon.StateMachine (Cmd(..), StateMachine, StateMachineF(..), State(..), runStateMachine)

data TimedController =
  TimedController (MVar TimedControllerCmd)

instance Controller TimedController where
  lock (TimedController m) =
    liftIO $
    putMVar m (ControllerCmd LockNowCmd)
  unlock (TimedController m) t =
    liftIO $
    putMVar m (ControllerCmd (UnlockCmd t))
  quit (TimedController m) =
    liftIO $
    do s <- newEmptyMVar
       putMVar m (Quit s)
       takeMVar s

-- | Create a new 'TimedController' using the given 'Lock.Lock' instance.
-- This will lock the 'Lock.Lock'.
initTimedController :: Lock.Lock l => l -> IO TimedController
initTimedController l = do
  Lock.lock l
  m <- newEmptyMVar
  _ <- forkIO (timedController m l Locked)
  return (TimedController m)

data TimedControllerCmd
  = ControllerCmd Cmd
  | Quit (MVar ())

-- | Note: don't expose this to the user of the controller. It's only
-- used for scheduled locks in response to unlock commands.
lockAt :: MVar TimedControllerCmd -> UTCTime -> IO ()
lockAt m t = putMVar m (ControllerCmd (LockCmd t))

timedController :: Lock.Lock l => MVar TimedControllerCmd -> l -> State -> IO ()
timedController m l = loop
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
