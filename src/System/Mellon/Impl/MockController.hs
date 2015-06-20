{-# LANGUAGE OverloadedStrings #-}

module System.Mellon.Impl.MockController
         ( runMockController
         ) where

import Control.Monad.Free (iterM)
import Control.Monad.IO.Class
import Data.Text (Text, pack)
import qualified Data.Text as T (concat)
import qualified Data.Text.IO as T (putStrLn)
import System.Mellon.Controller (Controller, ControllerF(..))

default (Text)

runMockController :: MonadIO m => Controller a -> m a
runMockController = iterM runMC
  where runMC :: MonadIO m => ControllerF (m a) -> m a

        runMC (Lock next) =
          do liftIO $ T.putStrLn "Locked"
             next

        runMC (Unlock next) =
          do liftIO $ T.putStrLn "Unlocked"
             next

        runMC (ScheduleLock atDate next) =
          do liftIO $ T.putStrLn $ T.concat ["Lock at ", pack $ show atDate]
             next

        runMC (UnscheduleLock next) =
          do liftIO $ T.putStrLn "Unschedule lock"
             next

