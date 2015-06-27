{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Mellon.Impl.MockLock
    ( MockLock
    , initMockLock
    ) where

import Data.Text (Text)
import qualified Data.Text.IO as T (putStrLn)
import System.Mellon.Lock (Lock(..))

default (Text)

data MockLock = MockLock

initMockLock :: IO MockLock
initMockLock = return MockLock

instance Lock IO MockLock where
  lock = const $ T.putStrLn "Locked"
  unlock = const $ T.putStrLn "Unlocked"


