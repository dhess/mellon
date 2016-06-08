-- | Run a @mellon-web@ server which wraps a GPIO-driven physical
-- access device.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Mellon.Controller (Device(..), controller)
import Mellon.Device.GPIO (sysfsGpioDevice)
import Mellon.Web.Server.DocsAPI (docsApp)
import Network (PortID(..), listenOn)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import Options.Applicative
import System.GPIO.Monad
       (Pin(..), PinActiveLevel(..), PinValue(..), PinOutputMode(..),
        withOutputPin)
import System.GPIO.Linux.Sysfs (runSysfsGpioIO)

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

runTCPServerSysfs :: Pin -> Int -> IO ()
runTCPServerSysfs pin port = runSysfsGpioIO $
  withOutputPin pin OutputDefault (Just ActiveHigh) Low $ \p ->
     liftIO $ runTCPServer (sysfsGpioDevice p) port

runTCPServer :: Device d -> Int -> IO ()
runTCPServer device port =
  do cc <- controller device
     sock <- listenOn (PortNumber (fromIntegral port))
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (docsApp cc)

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
                progDesc "A Mellon server for controlling physical access devices via GPIO" <>
                header "gpio-mellon-server")
