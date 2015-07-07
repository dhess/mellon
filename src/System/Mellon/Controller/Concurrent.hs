module System.Mellon.Controller.Concurrent
         ( ConcurrentController(..)
         , ConcurrentControllerCmd(..)
         , forkCC
         , lockAt
         ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class
import Data.Time (UTCTime)
import System.Mellon.Controller.Controller (Controller(..))
import System.Mellon.StateMachine (Cmd(..), State(..))

data ConcurrentController =
  ConcurrentController (MVar ConcurrentControllerCmd)

instance Controller ConcurrentController where
  lock (ConcurrentController m) =
    liftIO $
    putMVar m (ControllerCmd LockNowCmd)
  unlock (ConcurrentController m) t =
    liftIO $
    putMVar m (ControllerCmd (UnlockCmd t))
  quit (ConcurrentController m) =
    liftIO $
    do s <- newEmptyMVar
       putMVar m (Quit s)
       takeMVar s

data ConcurrentControllerCmd
  = ControllerCmd Cmd
  | Quit (MVar ())

forkCC :: (MVar ConcurrentControllerCmd -> State -> IO ()) -> IO ConcurrentController
forkCC loop = do
  m <- newEmptyMVar
  _ <- forkIO (loop m Locked)
  return (ConcurrentController m)

-- | Note: don't expose this to the user of the controller. It's only
-- used for scheduled locks in response to unlock commands.
lockAt :: MVar ConcurrentControllerCmd -> UTCTime -> IO ()
lockAt m t = putMVar m (ControllerCmd (LockCmd t))
