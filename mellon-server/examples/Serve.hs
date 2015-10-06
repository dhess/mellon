-- Compile from top-level with:
-- ghc -i src/Mellon/Server examples/Serve

module Main where

import Mellon.Controller
import Mellon.Lock.Mock
import Mellon.Server (app)
import Network.Wai.Handler.Warp

main :: IO ()
main =
  do ml <- mockLock
     cc <- concurrentControllerCtx ml
     putStrLn "Running on port 8081"
     run 8081 $ app cc

