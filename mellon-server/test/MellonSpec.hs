{-# LANGUAGE OverloadedStrings #-}

module MellonSpec (spec) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import qualified Mellon.Controller as MC
import Mellon.Lock.Mock
import Mellon.Server (app)
import Network.Wai.Handler.Warp
import qualified ServerTests as Tests (spec)
import Test.Hspec

sleep :: Int -> IO ()
sleep = threadDelay . (* 1000000)

startServer :: IO ()
startServer =
  do ml <- mockLock
     cc <- MC.concurrentControllerCtx ml
     _ <- forkIO (run 8081 $ app cc)
     sleep 10
     return ()

spec :: Spec
spec = beforeAll_ startServer Tests.spec

