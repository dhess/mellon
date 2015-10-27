{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances   #-}
#endif
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MellonClientSpec (spec) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Monad.Trans.Either (runEitherT)
import Data.Proxy
import Data.Time.Clock
import GHC.TypeLits
import qualified Mellon.Controller as MC
import Mellon.Lock.Mock
import Mellon.Client
import Mellon.Server (app)
import Network.Socket
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Client
import Test.Hspec

-- Portions of this code are:
--
-- Copyright (c) 2014, Zalora South East Asia Pte Ltd
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Zalora South East Asia Pte Ltd nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

runApp :: IO (ThreadId, BaseUrl)
runApp =
  do ml <- mockLock
     cc <- MC.concurrentControllerCtx ml
     (port, sock) <- openTestSocket
     let settings = setPort port $ defaultSettings
     threadId <- forkIO $ runSettingsSocket settings sock (app cc)
     return (threadId, BaseUrl Http "localhost" port)

killApp :: (ThreadId, BaseUrl) -> IO ()
killApp (threadId, _) = killThread threadId

spec :: Spec
spec = before runApp $ after killApp $
  do describe "getTime" $
       do it "returns the server time" $ \(_, baseUrl) ->
            do let getTime = getNth (Proxy :: Proxy 0) $ generateClientFunctions baseUrl
               now <- getCurrentTime
               Right (Time serverTime) <- runEitherT getTime
               let delta = 1.0 :: NominalDiffTime
               ((serverTime `diffUTCTime` now) < delta) `shouldBe` True

     describe "getState" $
       do it "returns the current server state" $ \(_, baseUrl) ->
            do let getState = getNth (Proxy :: Proxy 1) $ generateClientFunctions baseUrl
               Right state <- runEitherT getState
               state `shouldBe` Locked

     describe "putState" $
       do it "locking when the server is locked is idempotent" $ \(_, baseUrl) ->
            do let putState = getLast $ generateClientFunctions baseUrl
               Right state <- runEitherT $ putState Locked
               state `shouldBe` Locked

          it "can unlock the server until a specified date" $ \(_, baseUrl) ->
            do let putState = getLast $ generateClientFunctions baseUrl
               now <- getCurrentTime
               let untilTime = 3.0 `addUTCTime` now
               Right state <- runEitherT $ putState (Unlocked untilTime)
               state `shouldBe` (Unlocked untilTime)
               sleep 3
               let getState = getNth (Proxy :: Proxy 1) $ generateClientFunctions baseUrl
               Right newState <- runEitherT getState
               newState `shouldBe` Locked

          it "later unlocks override unlocks that expire earlier" $ \(_, baseUrl) ->
            do let putState = getLast $ generateClientFunctions baseUrl
               now <- getCurrentTime
               let untilTime = 3.0 `addUTCTime` now
               _ <- runEitherT $ putState (Unlocked untilTime)
               sleep 1
               let newUntilTime = 20.0 `addUTCTime` now
               Right state <- runEitherT $ putState (Unlocked newUntilTime)
               state `shouldBe` Unlocked newUntilTime

          it "locks override unlocks" $ \(_, baseUrl) ->
            do let putState = getLast $ generateClientFunctions baseUrl
               now <- getCurrentTime
               let untilTime = 3.0 `addUTCTime` now
               _ <- runEitherT $ putState (Unlocked untilTime)
               sleep 1
               Right state <- runEitherT $ putState Locked
               state `shouldBe` Locked

class GetNth (n :: Nat) a b | n a -> b where
    getNth :: Proxy n -> a -> b

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  GetNth 0 (x :<|> y) x where
      getNth _ (x :<|> _) = x

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  (GetNth (n - 1) x y) => GetNth n (a :<|> x) y where
      getNth _ (_ :<|> x) = getNth (Proxy :: Proxy (n - 1)) x

class GetLast a b | a -> b where
    getLast :: a -> b

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  (GetLast b c) => GetLast (a :<|> b) c where
      getLast (_ :<|> b) = getLast b

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  GetLast a a where
      getLast a = a
