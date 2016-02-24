{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Mellon.Gpio
         ( -- * Servers
           runTCPServerSysfs
           -- * Exceptions
         , ServerException(..)
         ) where

import Control.Monad.Catch (MonadThrow, Exception, throwM)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (Typeable)
import Mellon.Monad.Controller (controllerCtx)
import Mellon.Device.Class (Device(..))
import Mellon.Server.Docs (docsApp)
import Network (PortID(..), listenOn)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import System.GPIO.Free (GpioT, getPinDirection, withPin, writePin, writePin')
import System.GPIO.Linux.Sysfs (PinDescriptor)
import System.GPIO.Linux.Sysfs.IO (runSysfsIO)
import System.GPIO.Types (Pin(..), PinValue(..))

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
  do prepPin pin d
     liftIO $ runTCPServer (UnsafeSysfsLock d) port

runTCPServer :: (Device d) => d -> Int -> IO ()
runTCPServer device port =
  do cc <- controllerCtx device
     sock <- listenOn (PortNumber (fromIntegral port))
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (docsApp cc)

prepPin :: (MonadThrow m) => Pin -> h -> GpioT h m m ()
prepPin pin h =
  do maybeDir <- getPinDirection h
     case maybeDir of
       Nothing -> throwM $ NotAnOutputPin pin
       Just _ ->
         do writePin' h Low

data UnsafeSysfsLock = UnsafeSysfsLock PinDescriptor deriving (Show, Eq)

-- Note: this will throw an exception if there's a problem writing to
-- the pin.
instance Device UnsafeSysfsLock where
  lockDevice (UnsafeSysfsLock pd) = runSysfsIO $ writePin pd Low
  unlockDevice (UnsafeSysfsLock pd) = runSysfsIO $ writePin pd High

-- | Exceptions that can be thrown by the server.
data ServerException
  = NotAnOutputPin Pin
  deriving (Show,Typeable)

instance Exception ServerException
