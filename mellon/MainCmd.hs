module Main where

import Control.Concurrent (threadDelay)
import Data.Time (NominalDiffTime, UTCTime, TimeZone, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Options.Applicative
import System.Exit (exitSuccess)
import System.Mellon (Controller, initThreadedController, initTimedController, initMockLock, lock, quit, unlock)
import qualified System.Mellon.Controller.NewController as New (Controller, unlockUntil, lockNow)
import System.Mellon.Controller.NewConcurrent

data Verbosity
  = Normal
  | Verbose

data GlobalOptions =
  GlobalOptions {quiet :: Bool
                ,verbose :: Verbosity
                ,cmd :: Command}

data Command
  = Threaded ThreadedOptions
  | Timed TimedOptions
  | New NewOptions

data ThreadedOptions = ThreadedOptions {unusedThreaded :: Maybe String}

threadedCmd :: Parser Command
threadedCmd = Threaded <$> threadedOptions

threadedOptions :: Parser ThreadedOptions
threadedOptions =
  ThreadedOptions <$>
  optional (strOption (help "unused"))

data TimedOptions = TimedOptions {unusedTimed :: Maybe String}

timedCmd :: Parser Command
timedCmd = Timed <$> timedOptions

timedOptions :: Parser TimedOptions
timedOptions =
  TimedOptions <$>
  optional (strOption (help "unused"))

data NewOptions = NewOptions {unusedNew :: Maybe String}

newCmd :: Parser Command
newCmd = New <$> newOptions

newOptions :: Parser NewOptions
newOptions =
  NewOptions <$>
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
    (command "threaded" (info threadedCmd (progDesc "Run the threaded controller test")) <>
     command "timed" (info timedCmd (progDesc "Run the timed controller test")) <>
     command "new" (info newCmd (progDesc "Run the new controller test")))

putStrLnWithTime :: UTCTime -> TimeZone -> String -> IO ()
putStrLnWithTime t tz msg = putStrLn $ concat [formatTime defaultTimeLocale "%I:%M:%S %p" (utcToLocalTime tz t), " -- ", msg]

test :: Controller c => c -> IO ()
test c =
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
     unlock c $ (5 :: NominalDiffTime) `addUTCTime` tPlus2
     threadDelay (8 * 1000000)
     tPlus10 <- getCurrentTime
     unlock c $ (3 :: NominalDiffTime) `addUTCTime` tPlus10
     threadDelay (1 * 1000000)
     tPlus11 <- getCurrentTime
     unlock c $ (10 :: NominalDiffTime) `addUTCTime` tPlus11
     threadDelay (14 * 1000000)
     tPlus25 <- getCurrentTime
     unlock c $ (8 :: NominalDiffTime) `addUTCTime` tPlus25
     threadDelay (2 * 1000000)
     tPlus27 <- getCurrentTime
     unlock c $ (1 :: NominalDiffTime) `addUTCTime` tPlus27
     threadDelay (13 * 1000000)
     tPlus40 <- getCurrentTime
     unlock c $ (8 :: NominalDiffTime) `addUTCTime` tPlus40
     threadDelay (3 * 1000000)
     lock c
     threadDelay (12 * 1000000)
     quit c
     exitSuccess

testNew :: UTCTime -> New.Controller ()
testNew start =
  do New.unlockUntil $ (5 :: NominalDiffTime) `addUTCTime` start
     New.lockNow

run :: GlobalOptions -> IO ()
run (GlobalOptions False _ (Threaded _)) =
  do lck <- initMockLock
     c <- initThreadedController lck
     test c
run (GlobalOptions False _ (Timed _)) =
  do lck <- initMockLock
     c <- initTimedController lck
     test c
run (GlobalOptions False _ (New _)) =
  do now <- getCurrentTime
     runController $ testNew now
run _ = return ()

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Mellon electric strike controller" <>
                header "mellon - a command-based CLI for mellon")
