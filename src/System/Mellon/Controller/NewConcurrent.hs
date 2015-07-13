module System.Mellon.Controller.NewConcurrent
         ( runConcurrentControllerT
         ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Trans.Free (iterM, iterT)
import Control.Monad.IO.Class
import System.Mellon.Controller.NewController (Controller, ControllerF(..), ControllerT)
import System.Mellon.NewStateMachine (Cmd(..), State(..), StateMachine, StateMachineF(..), stateMachine)

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
          do liftIO $ putStrLn "ScheduleLock"
             next
        runSM (Unlock next) =
          do liftIO $ putStrLn "Unlock"
             next
        runSM (UnscheduleLock next) =
          do liftIO $ putStrLn "UnscheduleLock"
             next
        runSM (WaitForCmd next) =
          do cmd <- liftIO $ takeMVar m
             next cmd
