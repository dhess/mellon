module Main where

import Data.Time (getCurrentTime)
import Options.Applicative
import System.Mellon (initMockController, initMockLock, lockMockController, unlockMockController)

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
  do now <- getCurrentTime
     lck <- initMockLock
     mc <- initMockController lck
     lockMockController mc
     unlockMockController mc now
     return ()
run _ = return ()

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Mellon electric strike controller" <>
                header "mellon - a command-based CLI for mellon")
