module Main where

import Control.Monad.IO.Class (liftIO)
import Mellon.Gpio (runTCPServerSysfs)
import Options.Applicative
import System.GPIO.Free (openPin, withPin)
import System.GPIO.Linux.Sysfs.IO (runSysfsIO)
import System.GPIO.Types (Pin(..))

data GlobalOptions =
  GlobalOptions {_port :: Int
                ,_unexport :: Bool
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
  switch (long "unexport" <>
          short 'u' <>
          help "Unexport pin upon exit") <*>
  hsubparser
    (command "sysfs" (info sysfsCmd (progDesc "Use the Linux sysfs GPIO interpreter")))

run :: GlobalOptions -> IO ()
run (GlobalOptions listenPort False (Sysfs (SysfsOptions pinNumber))) =
  let pin = Pin pinNumber
  in
    do pd <- runSysfsIO $ openPin pin
       runTCPServerSysfs pin pd listenPort
run (GlobalOptions listenPort True (Sysfs (SysfsOptions pinNumber))) =
  let pin = Pin pinNumber
  in
    runSysfsIO $ withPin pin $ \pd ->
      liftIO $ runTCPServerSysfs pin pd listenPort

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "A server for controlling physical access devices" <>
                header "mellon-gpio")
