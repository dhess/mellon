module Main where

import Control.Concurrent (threadDelay)
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Options.Applicative
import System.Mellon (initThreadedController, initMockLock, lock, unlock)

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

run :: GlobalOptions -> IO ()
run (GlobalOptions False _ (Mock _)) =
  do lck <- initMockLock
     c <- initThreadedController lck
     putStrLn "Lock, wait 2 seconds, unlock for 5 seconds, quit about 3 seconds later."
     lock c
     threadDelay (2 * 1000000)
     now <- getCurrentTime
     unlock c $ (5 :: NominalDiffTime) `addUTCTime` now
     threadDelay (8 * 1000000)
     return ()
run _ = return ()

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Mellon electric strike controller" <>
                header "mellon - a command-based CLI for mellon")
