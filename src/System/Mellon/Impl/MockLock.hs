{-# LANGUAGE OverloadedStrings #-}

module System.Mellon.Impl.MockLock
    ( MockLock
    , initMockLock
    ) where

import Control.Monad.IO.Class
import Data.Text (Text, pack)
import qualified Data.Text as T (concat)
import qualified Data.Text.IO as T (putStrLn)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import System.Mellon.Lock (Lock(..))

default (Text)

data MockLock = MockLock

initMockLock :: IO MockLock
initMockLock = return MockLock

instance Lock MockLock where
  lock = const $
    do now <- liftIO $ currentLocalTimeAsText
       liftIO $ T.putStrLn $ T.concat ["Locked at ", now]
  unlock = const $
    do now <- liftIO $ currentLocalTimeAsText
       liftIO $ T.putStrLn $ T.concat ["Unlocked at ", now]

currentLocalTimeAsText :: IO Text
currentLocalTimeAsText =
  do now <- getCurrentTime
     tz <- getCurrentTimeZone
     return $ pack $ formatTime defaultTimeLocale "%I:%M:%S %p" (utcToLocalTime tz now)
