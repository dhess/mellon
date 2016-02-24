module Main where

import Mellon.Gpio (runTCPServerSysfs)
import Options.Applicative
import System.GPIO.Types (Pin(..))

data Verbosity
  = Normal
  | Verbose

data GlobalOptions =
  GlobalOptions {quiet :: Bool
                ,verbose :: Verbosity
                ,port :: Int
                ,cmd :: Command}

data Command
  = Sysfs SysfsOptions

data SysfsOptions = SysfsOptions {pin :: Int}

sysfsCmd :: Parser Command
sysfsCmd = Sysfs <$> sysfsOptions

sysfsOptions :: Parser SysfsOptions
sysfsOptions =
  SysfsOptions <$>
  argument auto (metavar "PIN" <>
                 help "GPIO pin number")

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
  option auto (long "port" <>
               short 'p' <>
               metavar "INT" <>
               value 8000 <>
               help "Listen on port") <*>
  hsubparser
    (command "sysfs" (info sysfsCmd (progDesc "Use the Linux sysfs GPIO interpreter")))

run :: GlobalOptions -> IO ()
run (GlobalOptions False _ listenPort (Sysfs (SysfsOptions pinNumber))) =
  runTCPServerSysfs (Pin pinNumber) listenPort
run _ = return ()

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "A server for controlling physical access devices" <>
                header "mellon-gpio")
