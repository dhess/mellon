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
     runConcurrentControllerT' m block

runConcurrentControllerT' :: MonadIO m => MVar Cmd -> ControllerT m a -> m a
runConcurrentControllerT' m' = iterT $ run m'
  where run :: MonadIO m => MVar Cmd -> ControllerF (m a) -> m a
        run m (LockNow next) =
          do liftIO $ putMVar m LockNowCmd
             next
        run m (UnlockUntil untilDate next) =
          do liftIO $ putMVar m (UnlockCmd untilDate)
             next

forkStateMachine :: MVar Cmd -> IO ()
forkStateMachine m = forkIO (runStateMachine' m (stateMachine Locked)) >> return ()

runStateMachine' :: (MonadIO m) => MVar Cmd -> StateMachine a -> m a
runStateMachine' m' = iterM $ run m'
  where run :: MonadIO m
            => MVar Cmd -> StateMachineF (m a) -> m a
        run _ (Lock next) =
          do liftIO $ putStrLn "Lock"
             next
        run _ (ScheduleLock atDate next) =
          do liftIO $ putStrLn "ScheduleLock"
             next
        run _ (Unlock next) =
          do liftIO $ putStrLn "Unlock"
             next
        run _ (UnscheduleLock next) =
          do liftIO $ putStrLn "UnscheduleLock"
             next
        run m (WaitForCmd next) =
          do cmd <- liftIO $ takeMVar m
             next cmd
