{-# LANGUAGE OverloadedStrings #-}

module MellonSpec (main, spec) where

import Control.Concurrent (forkIO, threadDelay)
import Data.Aeson
import Data.Time.Clock
import qualified Mellon.Controller as MC
import Mellon.Lock.Mock
import Mellon.Server (app, State(..))
import Network.Wai.Handler.Warp
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Test.Hspec

sleep :: Int -> IO ()
sleep = threadDelay . (* 1000000)

main :: IO ()
main = hspec spec

startServer :: IO Bool
startServer =
  do ml <- mockLock
     cc <- MC.concurrentControllerCtx ml
     _ <- forkIO (run 8081 $ app cc)
     -- Give the server time to spin up, and the user a chance to
     -- accept connections on OS X.
     sleep 10
     return True

initialState :: IO (Status, Maybe State)
initialState =
  do manager <- newManager defaultManagerSettings
     initialRequest <- parseUrl "http://localhost:8081/state"
     let request = initialRequest { method = "GET" }
     response <- httpLbs request manager
     return (responseStatus response, decode $ responseBody response)

serverTime :: IO (Status, Maybe UTCTime)
serverTime =
  do manager <- newManager defaultManagerSettings
     initialRequest <- parseUrl "http://localhost:8081/time"
     let request = initialRequest { method = "GET" }
     response <- httpLbs request manager
     return (responseStatus response, decode $ responseBody response)

spec :: Spec
spec = do
  describe "Running app" $ do
    it "should start the server" $ do
      startServer >>= (shouldBe True)

  -- We assume these tests run in the specified order, as the same
  -- server is used for each test and the server obviously is
  -- stateful.

  describe "Initial server state" $ do
    it "should be locked" $ do
      initialState >>= (shouldBe (ok200, Just Locked))

  describe "Server time" $ do
    it "should be accurate" $ do
      now <- getCurrentTime
      (code, Just time) <- serverTime
      code `shouldBe` ok200
      let delta = 1.0 :: NominalDiffTime
      ((time `diffUTCTime` now) < delta) `shouldBe` True
