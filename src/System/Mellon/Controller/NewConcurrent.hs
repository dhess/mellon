module System.Mellon.Controller.NewConcurrent
         ( runController
         ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Trans.Free (iterM)
import Control.Monad.IO.Class
import System.Mellon.Controller.NewController (Controller, ControllerF(..))
import System.Mellon.NewStateMachine (Cmd(..), State(..), StateMachine, StateMachineF(..), stateMachine)

runController :: MonadIO m => Controller a -> m a
runController block =
  do m <- liftIO newEmptyMVar
     liftIO $ forkStateMachine m
     runController' m block

runController' :: MonadIO m => MVar Cmd -> Controller a -> m a
runController' m' = iterM $ run m'
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
