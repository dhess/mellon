module Mellon.Web.ClientSpec (spec) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Data.Time.Clock
import Mellon.Controller (controller)
import Mellon.Device (mockLock, mockLockDevice)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.Socket
import Network.Wai.Handler.Warp
import Servant.Client
import Test.Hspec

import Mellon.Web.Client
import Mellon.Web.Server (app)

sleep :: Int -> IO ()
sleep = threadDelay . (* 1000000)

openTestSocket :: IO (Port, Socket)
openTestSocket = do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  port <- socketPort s
  return (fromIntegral port, s)

runApp :: IO (ThreadId, ClientEnv)
runApp =
  do ml <- mockLock
     cc <- controller Nothing $ mockLockDevice ml
     (port, sock) <- openTestSocket
     let settings = setPort port $ defaultSettings
     threadId <- forkIO $ runSettingsSocket settings sock (app cc)
     manager <- newManager defaultManagerSettings
     let clientEnv = ClientEnv manager $ BaseUrl Http "localhost" port ""
     return (threadId, clientEnv)

killApp :: (ThreadId, ClientEnv) -> IO ()
killApp (threadId, _) = killThread threadId

spec :: Spec
spec = before runApp $ after killApp $
  do describe "getTime" $
       do it "returns the server time" $ \(_, clientEnv) ->
            do now <- getCurrentTime
               Right (Time serverTime) <- runClientM getTime clientEnv
               let delta = 2.0 :: NominalDiffTime
               ((serverTime `diffUTCTime` now) < delta) `shouldBe` True

     describe "getState" $
       do it "returns the current server state" $ \(_, clientEnv) ->
            do Right state <- runClientM getState clientEnv
               state `shouldBe` Locked

     describe "putState" $
       do it "locking when the server is locked is idempotent" $ \(_, clientEnv) ->
            do Right state <- runClientM (putState Locked) clientEnv
               state `shouldBe` Locked

          it "can unlock the server until a specified date" $ \(_, clientEnv) ->
            do now <- getCurrentTime
               let untilTime = 3.0 `addUTCTime` now
               Right state <- runClientM (putState (Unlocked untilTime)) clientEnv
               state `shouldBe` (Unlocked untilTime)
               sleep 3
               Right newState <- runClientM getState clientEnv
               newState `shouldBe` Locked

          it "later unlocks override unlocks that expire earlier" $ \(_, clientEnv) ->
            do now <- getCurrentTime
               let untilTime = 3.0 `addUTCTime` now
               _ <- runClientM (putState (Unlocked untilTime)) clientEnv
               sleep 1
               let newUntilTime = 20.0 `addUTCTime` now
               Right state <- runClientM (putState (Unlocked newUntilTime)) clientEnv
               state `shouldBe` Unlocked newUntilTime

          it "locks override unlocks" $ \(_, clientEnv) ->
            do now <- getCurrentTime
               let untilTime = 3.0 `addUTCTime` now
               _ <- runClientM (putState (Unlocked untilTime)) clientEnv
               sleep 1
               Right state <- runClientM (putState Locked ) clientEnv
               state `shouldBe` Locked
