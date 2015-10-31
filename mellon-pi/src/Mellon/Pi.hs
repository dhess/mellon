{-# LANGUAGE OverloadedStrings #-}

module Mellon.Pi
         ( Pin(..)
         , runTCPServer
         ) where

import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Mellon.Controller (concurrentControllerCtx)
import Mellon.Lock (LockDevice(..))
import Mellon.Server.Docs (docsApp)
import Network (PortID(..), listenOn)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)

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


-- Placeholders until I've integrated HPi.

withGPIO :: IO a -> IO a
withGPIO = id

data Pin =
            Pin03|Pin05|Pin07|Pin08|Pin10|Pin11|Pin12|Pin13|Pin15|Pin16|Pin18|Pin19|Pin21|Pin22|Pin23|Pin24|Pin26|
            PinP5_03|PinP5_04|PinP5_05|PinP5_06|
            PinV1_03|PinV1_05|PinV1_07|PinV1_08|PinV1_10|PinV1_11|PinV1_12|PinV1_13|PinV1_15|PinV1_16|PinV1_18|PinV1_19|PinV1_21|
            PinV1_22|PinV1_23|PinV1_24|PinV1_26
            deriving (Eq,Show)

data PinMode = Input | Output | Alt0 | Alt1 | Alt2 | Alt3 | Alt4 | Alt5 deriving (Eq,Show)

instance Enum PinMode where
    fromEnum = fromJust . flip lookup table
    toEnum = fromJust . flip lookup (map swap table)

table :: [(PinMode, Int)]
table = [(Input, 0), (Output, 1), (Alt0, 4), (Alt1, 5), (Alt2, 6), (Alt3, 7), (Alt4, 3), (Alt5, 2)]

type LogicLevel = Bool

setPinFunction :: Pin -> PinMode -> IO ()
setPinFunction _ _ = return ()

writePin :: Pin -> LogicLevel -> IO ()
writePin _ _ = return ()
