{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Mellon.Gpio
         ( -- * Servers
           runTCPServerSysfs
         ) where

import Control.Monad.IO.Class (liftIO)
import Mellon.Monad.Controller (controllerCtx)
import Mellon.Device.Class (Device(..))
import Mellon.Server.Docs (docsApp)
import Network (PortID(..), listenOn)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import System.GPIO.Monad
       (Pin(..), PinActiveLevel(..), PinValue(..), PinOutputMode(..),
        withOutputPin)
import System.GPIO.Linux.Sysfs (runSysfsGpioIO)

-- | Run the server on a TCP socket at the given port number. The
-- server will listen on all interfaces.
--
-- The server will control the physical access device by assering a
-- high logic level on the specified GPIO pin when the device is
-- unlocked, and a low logic level on the pin when the device is
-- locked.
runTCPServerSysfs :: Pin -> Int -> IO ()
runTCPServerSysfs pin port =
  do prepPin pin
     liftIO $ runTCPServer (UnsafeSysfsLock pin) port

runTCPServer :: (Device d) => d -> Int -> IO ()
runTCPServer device port =
  do cc <- controllerCtx device
     sock <- listenOn (PortNumber (fromIntegral port))
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (docsApp cc)

prepPin :: Pin -> IO ()
prepPin pin = runSysfsGpioIO $
  withOutputPin pin OutputDefault (Just ActiveHigh) Low (const $ return ())

data UnsafeSysfsLock = UnsafeSysfsLock Pin deriving (Show, Eq)

-- Note: this will throw an exception if there's a problem writing to
-- the pin.
instance Device UnsafeSysfsLock where
  lockDevice (UnsafeSysfsLock pin) = runSysfsGpioIO $
    withOutputPin pin OutputDefault (Just ActiveHigh) Low (const $ return ())
  unlockDevice (UnsafeSysfsLock pin) = runSysfsGpioIO $
    withOutputPin pin OutputDefault (Just ActiveHigh) High (const $ return ())
