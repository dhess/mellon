-- Compile from top-level with "ghc -isrc"

module Main where

import Mellon.Controller (controller)
import Mellon.Device (mockLock, mockLockDevice)
import Mellon.Server.DocsAPI (docsApp)
import Network.Wai.Handler.Warp

main :: IO ()
main =
  do ml <- mockLock
     cc <- controller $ mockLockDevice ml
     putStrLn "Running on port 8081"
     run 8081 $ docsApp cc

