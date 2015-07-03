module Main where

import Control.Concurrent (threadDelay)
import Data.Time (NominalDiffTime, UTCTime, TimeZone, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Options.Applicative
import System.Mellon (initThreadedController, initMockLock, lock, quit, unlock)

data Verbosity
  = Normal
  | Verbose

data GlobalOptions =
  GlobalOptions {quiet :: Bool
                ,verbose :: Verbosity
                ,cmd :: Command}

data Command
  = Mock MockOptions

data MockOptions = MockOptions {unused :: Maybe String}

mockCmd :: Parser Command
mockCmd = Mock <$> mockOptions

mockOptions :: Parser MockOptions
mockOptions =
  MockOptions <$>
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
    (command "mock" (info mockCmd (progDesc "Run the mock controller")))

putStrLnWithTime :: UTCTime -> TimeZone -> String -> IO ()
putStrLnWithTime t tz msg = putStrLn $ concat [formatTime defaultTimeLocale "%I:%M:%S %p" (utcToLocalTime tz t), " -- ", msg]

run :: GlobalOptions -> IO ()
run (GlobalOptions False _ (Mock _)) =
  do lck <- initMockLock
     tz <- getCurrentTimeZone
     now <- getCurrentTime
     putStrLn "The test to be run (times may vary a slight bit due to thread scheduling vagaries):"
     putStrLnWithTime now tz "Lock"
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
     c <- initThreadedController lck
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
     threadDelay (1 * 1000000)

run _ = return ()

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Mellon electric strike controller" <>
                header "mellon - a command-based CLI for mellon")
