-- | Run a @mellon-web@ server with a mock lock device on port 8081.

module Main where

import Protolude
import Mellon.Controller (controller)
import Mellon.Device (mockLock, mockLockDevice)
import Mellon.Web.Server (swaggerApp)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main =
  do ml <- mockLock
     cc <- controller Nothing $ mockLockDevice ml
     putStrLn "Running on port 8081"
     run 8081 $ swaggerApp cc
