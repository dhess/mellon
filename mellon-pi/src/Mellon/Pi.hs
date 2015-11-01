{-# LANGUAGE OverloadedStrings #-}

module Mellon.Pi
         ( -- * Re-exported from "System.RaspberryPi.GPIO"
	   Pin(..)
           -- * Servers
         , runTCPServer
         ) where

import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Mellon.Controller (concurrentControllerCtx)
import Mellon.Lock (LockDevice(..))
import Mellon.Server.Docs (docsApp)
import Network (PortID(..), listenOn)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)
import System.RaspberryPi.GPIO (Pin(..), PinMode(..), setPinFunction, withGPIO, writePin)

-- | Run the server on a TCP socket at the given port number. The
-- server will listen on all interfaces.
--
-- The server will control the physical access device by assering a
-- high logic level on the specified GPIO pin when the device is
-- unlocked, and a low logic level on the pin when the device is
-- locked. The server will also perform all necessary GPIO setup and
-- teardown for the specified pin (i.e., there is no need for the
-- caller to wrap this function with the 'withGPIO' function).
runTCPServer :: Pin -> Int -> IO ()
runTCPServer pin port = withGPIO $
  do setPinFunction pin Output
     cc <- concurrentControllerCtx (PiLock pin)
     sock <- listenOn (PortNumber (fromIntegral port))
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (docsApp cc)

data PiLock = PiLock Pin deriving (Show, Eq)

instance LockDevice PiLock where
  lockDevice (PiLock p) = writePin p True
  unlockDevice (PiLock p) = writePin p False
