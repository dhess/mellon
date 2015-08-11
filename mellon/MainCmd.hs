{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Concurrent as CC (forkIO, threadDelay)
import Control.Monad.IO.Class
import Data.Time (NominalDiffTime, UTCTime, TimeZone, addUTCTime, defaultTimeLocale, formatTime, utcToLocalTime)
import qualified Data.Time as Time (getCurrentTimeZone, getCurrentTime)
import Options.Applicative
import Prelude hiding (putStrLn)
import qualified Prelude as Prelude (putStrLn)
import System.Mellon.Controller (ConcurrentController, ControllerT, concurrentController, runConcurrentControllerT, runConcurrentStateMachine, unlockUntil, lockNow)
import System.Mellon.Lock (MonadLock(..), MockLockT, execMockLockT, runMockLockT)

data Verbosity
  = Normal
  | Verbose

data GlobalOptions =
  GlobalOptions {quiet :: Bool
                ,verbose :: Verbosity
                ,cmd :: Command}

data Command
  = Concurrent ConcurrentOptions
  | MockLockCmd MockLockOptions

data ConcurrentOptions = ConcurrentOptions {unusedConcurrent :: Maybe String}

concurrentCmd :: Parser Command
concurrentCmd = Concurrent <$> concurrentOptions

concurrentOptions :: Parser ConcurrentOptions
concurrentOptions =
  ConcurrentOptions <$>
  optional (strOption (help "unused"))

data MockLockOptions = MockLockOptions {unusedMockLock :: Maybe String}

mockLockCmd :: Parser Command
mockLockCmd = MockLockCmd <$> mockLockOptions

mockLockOptions :: Parser MockLockOptions
mockLockOptions =
  MockLockOptions <$>
  optional (strOption (help "unused"))

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
  switch (long "quiet" <>
          short 'q' <>
          help "Be quiet") <*>
  flag Normal
       Verbose
       (long "verbose" <>
        short 'v' <>
        help "Enable verbose mode") <*>
  hsubparser
    (command "concurrent" (info concurrentCmd (progDesc "Run the concurrent controller test")) <>
     command "mocklock" (info mockLockCmd (progDesc "Run the mock lock test")))

threadDelay :: MonadIO m => Int -> m ()
threadDelay = liftIO . CC.threadDelay

getCurrentTimeZone :: MonadIO m => m TimeZone
getCurrentTimeZone = liftIO Time.getCurrentTimeZone

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Time.getCurrentTime

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . Prelude.putStrLn

putStrLnWithTime :: MonadIO m => UTCTime -> TimeZone -> String -> m ()
putStrLnWithTime t tz msg = putStrLn $ concat [formatTime defaultTimeLocale "%I:%M:%S %p" (utcToLocalTime tz t), " -- ", msg]

testConcurrent :: ControllerT IO Int
testConcurrent =
  do tz <- getCurrentTimeZone
     now <- getCurrentTime

     putStrLn "The test to be run (times may vary a slight bit due to thread scheduling vagaries):"
     putStrLnWithTime ((2 :: NominalDiffTime) `addUTCTime` now) tz "Unlock for 5 seconds"
     putStrLnWithTime ((10 :: NominalDiffTime) `addUTCTime` now) tz "Unlock for 3 seconds"
     putStrLnWithTime ((11 :: NominalDiffTime) `addUTCTime` now) tz "Unlock for 10 seconds; this unlock should override the previous one"
     putStrLnWithTime ((25 :: NominalDiffTime) `addUTCTime` now) tz "Unlock for 8 seconds"
     putStrLnWithTime ((27 :: NominalDiffTime) `addUTCTime` now) tz "Unlock for 1 second; this unlock should be ignored"
     putStrLnWithTime ((40 :: NominalDiffTime) `addUTCTime` now) tz "Unlock for 8 seconds"
     putStrLnWithTime ((43 :: NominalDiffTime) `addUTCTime` now) tz "Lock immediately; this should unschedule the previous lock"
     putStrLnWithTime ((55 :: NominalDiffTime) `addUTCTime` now) tz "Quit"
     putStrLn ""
     putStrLn "Test begins now."
     threadDelay (2 * 1000000)
     tPlus2 <- getCurrentTime
     unlockUntil $ (5 :: NominalDiffTime) `addUTCTime` tPlus2
     threadDelay (8 * 1000000)
     tPlus10 <- getCurrentTime
     unlockUntil $ (3 :: NominalDiffTime) `addUTCTime` tPlus10
     threadDelay (1 * 1000000)
     tPlus11 <- getCurrentTime
     unlockUntil $ (10 :: NominalDiffTime) `addUTCTime` tPlus11
     threadDelay (14 * 1000000)
     tPlus25 <- getCurrentTime
     unlockUntil $ (8 :: NominalDiffTime) `addUTCTime` tPlus25
     threadDelay (2 * 1000000)
     tPlus27 <- getCurrentTime
     unlockUntil $ (1 :: NominalDiffTime) `addUTCTime` tPlus27
     threadDelay (13 * 1000000)
     tPlus40 <- getCurrentTime
     unlockUntil $ (8 :: NominalDiffTime) `addUTCTime` tPlus40
     threadDelay (3 * 1000000)
     lockNow
     threadDelay (12 * 1000000)
     return 7

runCCTest :: ConcurrentController Int -> IO ()
runCCTest cc =
  do _ <- runConcurrentControllerT cc testConcurrent
     return ()

testMockLock :: MockLockT IO ()
testMockLock =
  do lock
     unlock
     unlock
     lock

run :: GlobalOptions -> IO Int
run (GlobalOptions False _ (Concurrent _)) =
  do cc :: ConcurrentController Int <- concurrentController
     --_ <- CC.forkIO (evalMockLockT $ runConcurrentStateMachine cc ())
     --runConcurrentControllerT cc testConcurrent
     _ <- CC.forkIO (runCCTest cc)
     (result, lockEvents) <- runMockLockT $ runConcurrentStateMachine cc
     print (result, lockEvents)
     return result
run (GlobalOptions False _ (MockLockCmd _)) =
  do output <- execMockLockT testMockLock
     print output
     return 0
run _ = return 0

main :: IO Int
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Mellon electric strike controller" <>
                header "mellon - a command-based CLI for mellon")
