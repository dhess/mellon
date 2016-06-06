module MellonClientSpec (spec) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Monad.Trans.Except (runExceptT)
import Data.Time.Clock
import Mellon.Controller (controller)
import Mellon.Device (mockLock, mockLockDevice)
import Mellon.Server (app)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.Socket
import Network.Wai.Handler.Warp
import Servant.Client
import Test.Hspec

import Mellon.Client

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

runApp :: IO (ThreadId, Manager, BaseUrl)
runApp =
  do ml <- mockLock
     cc <- controller $ mockLockDevice ml
     (port, sock) <- openTestSocket
     let settings = setPort port $ defaultSettings
     threadId <- forkIO $ runSettingsSocket settings sock (app cc)
     manager <- newManager defaultManagerSettings
     return (threadId, manager, BaseUrl Http "localhost" port "")

killApp :: (ThreadId, Manager, BaseUrl) -> IO ()
killApp (threadId, _, _) = killThread threadId

spec :: Spec
spec = before runApp $ after killApp $
  do describe "getTime" $
       do it "returns the server time" $ \(_, manager, baseUrl) ->
            do now <- getCurrentTime
               Right (Time serverTime) <- runExceptT (getTime manager baseUrl)
               let delta = 1.0 :: NominalDiffTime
               ((serverTime `diffUTCTime` now) < delta) `shouldBe` True

     describe "getState" $
       do it "returns the current server state" $ \(_, manager, baseUrl) ->
            do Right state <- runExceptT (getState manager baseUrl)
               state `shouldBe` Locked

     describe "putState" $
       do it "locking when the server is locked is idempotent" $ \(_, manager, baseUrl) ->
            do Right state <- runExceptT $ putState Locked manager baseUrl
               state `shouldBe` Locked

          it "can unlock the server until a specified date" $ \(_, manager, baseUrl) ->
            do now <- getCurrentTime
               let untilTime = 3.0 `addUTCTime` now
               Right state <- runExceptT $ putState (Unlocked untilTime) manager baseUrl
               state `shouldBe` (Unlocked untilTime)
               sleep 3
               Right newState <- runExceptT (getState manager baseUrl)
               newState `shouldBe` Locked

          it "later unlocks override unlocks that expire earlier" $ \(_, manager, baseUrl) ->
            do now <- getCurrentTime
               let untilTime = 3.0 `addUTCTime` now
               _ <- runExceptT $ putState (Unlocked untilTime) manager baseUrl
               sleep 1
               let newUntilTime = 20.0 `addUTCTime` now
               Right state <- runExceptT $ putState (Unlocked newUntilTime) manager baseUrl
               state `shouldBe` Unlocked newUntilTime

          it "locks override unlocks" $ \(_, manager, baseUrl) ->
            do now <- getCurrentTime
               let untilTime = 3.0 `addUTCTime` now
               _ <- runExceptT $ putState (Unlocked untilTime) manager baseUrl
               sleep 1
               Right state <- runExceptT $ putState Locked manager baseUrl
               state `shouldBe` Locked
