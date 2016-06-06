{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Mellon.Gpio
         ( -- * Servers
           runTCPServerSysfs
         ) where

import Control.Monad.IO.Class (liftIO)
import Mellon.Controller (Device(..), controller)
import Mellon.Server.DocsAPI (docsApp)
import Network (PortID(..), listenOn)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import System.GPIO.Monad
       (Pin(..), PinActiveLevel(..), PinValue(..), PinOutputMode(..),
        OutputPin, withOutputPin, writeOutputPin)
import System.GPIO.Linux.Sysfs (PinDescriptor, runSysfsGpioIO)

-- | Run the server on a TCP socket at the given port number. The
-- server will listen on all interfaces.
--
-- The server will control the physical access device by assering a
-- high logic level on the specified GPIO pin when the device is
-- unlocked, and a low logic level on the pin when the device is
-- locked.
runTCPServerSysfs :: Pin -> Int -> IO ()
runTCPServerSysfs pin port = runSysfsGpioIO $
  withOutputPin pin OutputDefault (Just ActiveHigh) Low $ \p ->
     liftIO $ runTCPServer (unsafeSysfsLockDevice $ UnsafeSysfsLock p) port

runTCPServer :: Device d -> Int -> IO ()
runTCPServer device port =
  do cc <- controller device
     sock <- listenOn (PortNumber (fromIntegral port))
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (docsApp cc)

data UnsafeSysfsLock p = UnsafeSysfsLock p deriving (Show, Eq)

unsafeSysfsLockDevice :: UnsafeSysfsLock (OutputPin PinDescriptor) -> Device (UnsafeSysfsLock p)
unsafeSysfsLockDevice (UnsafeSysfsLock p) =
  Device (runSysfsGpioIO $ writeOutputPin p Low)
         (runSysfsGpioIO $ writeOutputPin p High)
