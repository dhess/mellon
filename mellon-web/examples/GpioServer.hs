-- | Run a @mellon-web@ server which wraps a GPIO-driven physical
-- access device.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (NominalDiffTime)
import Mellon.Controller (Device(..), controller)
import Mellon.Device.GPIO (sysfsGpioDevice)
import Mellon.Web.Server (swaggerApp)
import Network (PortID(..), listenOn)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import Options.Applicative
import System.GPIO.Monad
       (Pin(..), PinActiveLevel(..), PinValue(..), PinOutputMode(..),
        withOutputPin)
import System.GPIO.Linux.Sysfs (runSysfsGpioIO)

data GlobalOptions =
  GlobalOptions {_port :: !Int
                ,_minUnlockTime :: !Int
                ,_activeLow :: !Bool
                ,_cmd :: !Command}

data Command
  = Sysfs SysfsOptions

data SysfsOptions = SysfsOptions {_pin :: !Int}

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
  option auto (long "min-unlock-time" <>
               short 'u' <>
               metavar "INT" <>
               value 1 <>
               help "Minimum unlock time in seconds") <*>
  switch (long "active-low" <>
          short 'l' <>
          help "Pin is active low") <*>
  hsubparser
    (command "sysfs" (info sysfsCmd (progDesc "Use the Linux sysfs GPIO interpreter")))

runTCPServerSysfs :: Pin -> Int -> NominalDiffTime -> Bool -> IO ()
runTCPServerSysfs pin port minUnlockTime lowFlag = runSysfsGpioIO $
  withOutputPin pin OutputDefault (Just $ activeLevel lowFlag) Low $ \p ->
     liftIO $ runTCPServer minUnlockTime (sysfsGpioDevice p) port
  where
    activeLevel :: Bool -> PinActiveLevel
    activeLevel False = ActiveHigh
    activeLevel True = ActiveLow

runTCPServer :: NominalDiffTime -> Device d -> Int -> IO ()
runTCPServer minUnlock device port =
  do cc <- controller (Just minUnlock) device
     sock <- listenOn (PortNumber (fromIntegral port))
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (swaggerApp cc)

run :: GlobalOptions -> IO ()
run (GlobalOptions listenPort minUnlockTime activeLow (Sysfs (SysfsOptions pinNumber))) =
  let pin = Pin pinNumber
  in
    runTCPServerSysfs pin listenPort (fromIntegral minUnlockTime) activeLow

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "A Mellon server for controlling physical access devices via GPIO" <>
                header "gpio-mellon-server")
