{-# LANGUAGE OverloadedStrings #-}

module System.Mellon.Impl.MockLock
    ( MockLock
    , initMockLock
    ) where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text.IO as T (putStrLn)
import System.Mellon.Lock (Lock(..))

default (Text)

data MockLock = MockLock

initMockLock :: IO MockLock
initMockLock = return MockLock

instance Lock MockLock where
  lock = const $ liftIO $ T.putStrLn "Locked"
  unlock = const $ liftIO $ T.putStrLn "Unlocked"


