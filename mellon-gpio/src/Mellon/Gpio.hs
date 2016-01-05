{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Mellon.Gpio
         ( -- * Servers
           runTCPServerSysfs
         ) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (liftIO)
import Mellon.Monad.Controller (controllerCtx)
import Mellon.Device.Class (Device(..))
import Mellon.Server.Docs (docsApp)
import Network (PortID(..), listenOn)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import System.GPIO.Free (GpioT, Pin(..), PinValue(..), getPinDirection, withPin, writePin, writePin')
import System.GPIO.Linux.Sysfs (PinDescriptor)
import System.GPIO.Linux.SysfsIO (runSysfsIO)

-- | Run the server on a TCP socket at the given port number. The
-- server will listen on all interfaces.
--
-- The server will control the physical access device by assering a
-- high logic level on the specified GPIO pin when the device is
-- unlocked, and a low logic level on the pin when the device is
-- locked. The server will also perform all necessary GPIO setup and
-- teardown for the specified pin (i.e., there is no need for the
-- caller to wrap this function with the 'withGPIO' function).
runTCPServerSysfs :: Pin -> Int -> IO ()
runTCPServerSysfs pin port = runSysfsIO $ withPin pin $ \d ->
  do prepPin d
     liftIO $ runTCPServer (UnsafeSysfsLock d) port

runTCPServer :: (Device d) => d -> Int -> IO ()
runTCPServer device port =
  do cc <- controllerCtx device
     sock <- listenOn (PortNumber (fromIntegral port))
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (docsApp cc)

prepPin :: (MonadError String m) => h -> GpioT String h m m ()
prepPin h =
  do maybeDir <- getPinDirection h
     case maybeDir of
       Nothing -> throwError $ "Can't set pin direction"
       Just _ ->
         do writePin' h Low

data UnsafeSysfsLock = UnsafeSysfsLock PinDescriptor deriving (Show, Eq)

-- Note: this will throw an exception if there's a problem writing to
-- the pin.
instance Device UnsafeSysfsLock where
  lockDevice (UnsafeSysfsLock pd) = runSysfsIO $ writePin pd Low
  unlockDevice (UnsafeSysfsLock pd) = runSysfsIO $ writePin pd High
