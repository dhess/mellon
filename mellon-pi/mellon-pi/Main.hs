module Main where

import Mellon.Pi (Pin(..), runTCPServer)
import Options.Applicative

data Verbosity
  = Normal
  | Verbose

data GlobalOptions =
  GlobalOptions {quiet :: Bool
                ,verbose :: Verbosity
                ,cmd :: Command}

data Command
  = Start StartOptions

data StartOptions =
  StartOptions {port :: Int}

startCmd :: Parser Command
startCmd = Start <$> startOptions

startOptions :: Parser StartOptions
startOptions =
  StartOptions <$>
  option auto (long "port" <>
               short 'p' <>
               metavar "INT" <>
               value 8000 <>
               help "Listen on port")

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
    (command "start" (info startCmd (progDesc "Start the server")))

run :: GlobalOptions -> IO ()
run (GlobalOptions False _ (Start (StartOptions listenPort))) =
  runTCPServer Pin03 listenPort
run _ = return ()

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "A server for controlling physical access devices" <>
                header "mellon-pi")
