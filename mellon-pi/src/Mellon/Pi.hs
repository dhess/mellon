{-# LANGUAGE OverloadedStrings #-}

module Mellon.Pi
         ( runTCPServer
         ) where

import Mellon.Controller (concurrentControllerCtx)
import Mellon.Lock.Mock (mockLock)
import Mellon.Server.Docs (docsApp)
import Network (PortID(..), listenOn)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setHost, setPort)

runTCPServer :: Int -> IO ()
runTCPServer port =
  do ml <- mockLock
     cc <- concurrentControllerCtx ml
     sock <- listenOn (PortNumber (fromIntegral port))
     runSettingsSocket (setPort port $ setHost "*" defaultSettings) sock (docsApp cc)
