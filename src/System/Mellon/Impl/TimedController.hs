module System.Mellon.Impl.TimedController
         ( TimedController(..)
         , initTimedController
         ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay, tryTakeMVar)
import Control.Monad (when)
import Control.Monad.Free (iterM)
import Control.Monad.IO.Class
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import qualified System.Mellon.Lock as Lock (Lock(..))
import System.Mellon.Controller (Controller(..))
import System.Mellon.StateMachine (Cmd(..), StateMachine, StateMachineF(..), State(..), runStateMachine)

-- | 'TimedController' combines a 'Lock' with a periodic timer-based
-- scheduling system.
--
-- When a 'TimedController' is created via 'initTimedController', only
-- a single thread is created. This thread handles all user commands
-- and all scheduled locking. No other threads are created by that
-- 'TimedController' instance; and, after initialization, the
-- 'TimedController' runs in constant space. In severely
-- resource-constrained environments, this can be an advantage over
-- the 'ThreadedController' implementation.
--
-- The downside to 'TimedController' is that it only sleeps for
-- approximately 1 second before waking up again to check the for
-- incoming user commands or expiring unlocks. This wakeup occurs even
-- if there's nothing to do, in which case the controller immediately
-- goes back to sleep. In any case, this behavior will probably
-- prevent most platforms from entering a low-power state, which may
-- increase power usage compared to implementations that use
-- 'ThreadedController'; that controller implementation only runs when
-- it receives a command, or when an unlock has expired.
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

-- | Create a new 'TimedController'. This launches a new thread.
-- Communication is achived with the controller via its 'MVar'.
--
-- The newly-created controller will wake up once per second to check
-- for new commands or expired unlocks. This means that
-- 'TimedController's have a precision of approximately 1 second.
initTimedController :: Lock.Lock l => l -> IO TimedController
initTimedController l = do
  Lock.lock l
  m <- newEmptyMVar
  _ <- forkIO (timedController m l Locked)
  return (TimedController m)

-- | The controller's commands.
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
