module Main where

import Mellon.Gpio (runTCPServerSysfs)
import Options.Applicative
import System.GPIO.Types (Pin(..))

data GlobalOptions =
  GlobalOptions {_port :: Int
                ,_cmd :: Command}

data Command
  = Sysfs SysfsOptions

data SysfsOptions = SysfsOptions {_pin :: Int}

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
  option auto (long "port" <>
               short 'p' <>
               metavar "INT" <>
               value 8000 <>
               help "Listen on port") <*>
  hsubparser
    (command "sysfs" (info sysfsCmd (progDesc "Use the Linux sysfs GPIO interpreter")))

run :: GlobalOptions -> IO ()
run (GlobalOptions listenPort (Sysfs (SysfsOptions pinNumber))) =
  let pin = Pin pinNumber
  in
    runTCPServerSysfs pin listenPort

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "A server for controlling physical access devices" <>
                header "mellon-gpio")
