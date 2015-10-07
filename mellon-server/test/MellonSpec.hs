{-# LANGUAGE OverloadedStrings #-}

module MellonSpec (spec) where

import Control.Concurrent (ThreadId, killThread, forkIO)
import qualified Mellon.Controller as MC
import Mellon.Lock.Mock
import Mellon.Server (app, docsApp)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified ServerTests as Tests (docsSpec, spec, sleep)
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
  do describe "The MellonAPI server should work properly" $
       do tid <- runIO $ startServer app 10 -- 10s pause to give people on Macs time to permit connections
          afterAll_ (killThread tid) Tests.spec

     describe "The DocsAPI server should work properly" $
       do tid <- runIO $ startServer docsApp 1 -- 1s pause now just to let the server spin up
          afterAll_ (killThread tid) (Tests.spec >> Tests.docsSpec)
