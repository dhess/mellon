{-# LANGUAGE OverloadedStrings #-}

module MellonSpec (spec) where

import Control.Concurrent (ThreadId, killThread, forkIO)
import qualified Mellon.Controller as MC
import Mellon.Lock.Mock
import Mellon.Server (app)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified ClientTests as Tests (spec, sleep)
import Test.Hspec

startServer :: (MC.ConcurrentControllerCtx -> Application) -> Int -> IO ThreadId
startServer waiApp delay =
  do ml <- mockLock
     cc <- MC.concurrentControllerCtx ml
     tid <- forkIO (run 8081 $ waiApp cc)
     Tests.sleep delay
     return tid

spec :: Spec
spec =
  do describe "The MellonAPI client bindings should work properly" $
       do tid <- runIO $ startServer app 10 -- 10s pause to give people on Macs time to permit connections
          afterAll_ (killThread tid) Tests.spec
