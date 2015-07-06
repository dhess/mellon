{-# LANGUAGE OverloadedStrings #-}

-- | 'MockLock' is a dummy 'Lock' implementation that simply prints a
-- timestampped string to stdout upon receiving a command. It's useful
-- for testing, and not much else.

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

mockMsg :: Text -> IO ()
mockMsg actionText =
  do now <- currentLocalTimeAsText
     T.putStrLn $ T.concat [actionText, " at ", now]
  where
    currentLocalTimeAsText :: IO Text
    currentLocalTimeAsText =
      do now <- getCurrentTime
         tz <- getCurrentTimeZone
         return $ pack $ formatTime defaultTimeLocale "%I:%M:%S %p" (utcToLocalTime tz now)

instance Lock MockLock where
  lock = const $ liftIO $ mockMsg "Locked"
  unlock = const $ liftIO $ mockMsg "Unlocked"
  quit = const $ liftIO $ mockMsg "Quit"

