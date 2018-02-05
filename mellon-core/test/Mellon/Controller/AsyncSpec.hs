{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-incomplete-uni-patterns -fno-warn-unused-top-binds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Mellon.Controller.AsyncSpec (spec) where

import Protolude
import Control.Concurrent (MVar, newMVar, modifyMVar, threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (Exception(..))
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Strict (RWST, execRWST, ask, tell)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import qualified Data.Time as Time (getCurrentTime)
import Test.Hspec

import Mellon.Controller.Async
       (Controller, State(..), controller, minUnlockTime, lockController,
        queryController, unlockController)
import Mellon.Device
       (Device(..), MockLockEvent(..), events, mockLock, mockLockDevice)

sleep :: (MonadIO m) => Int -> m ()
sleep = liftIO . threadDelay . (* 1000000)

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Time.getCurrentTime

timePlusN :: UTCTime -> Integer -> UTCTime
timePlusN time n = (fromInteger n) `addUTCTime` time

type TestController d a = RWST (Controller d) [MockLockEvent] () IO a

testController :: Controller d -> IO [MockLockEvent]
testController ctrl =
  do (_, expectedResults) <- execRWST theTest ctrl ()
     return expectedResults

  where theTest :: TestController d ()
        theTest =
          do unlockWillExpire 5
             sleep 8
             unlockWontExpire 3
             sleep 1
             unlockExtend 10
             sleep 14
             unlockWillExpire 8
             sleep 2
             unlockWillBeIgnored 1
             sleep 13
             unlockWontExpire 8
             sleep 3
             lockIt
             sleep 12

        lockIt :: TestController d ()
        lockIt =
          do now <- getCurrentTime
             cc <- ask
             void $ lockController cc
             tell [LockEvent now]

        unlockIt :: Integer -> TestController d (UTCTime, UTCTime)
        unlockIt duration =
          do now <- getCurrentTime
             cc <- ask
             let expire = timePlusN now duration
             void $ unlockController expire cc
             return (now, expire)

        unlockWillExpire :: Integer -> TestController d ()
        unlockWillExpire duration =
          do (now, expire) <- unlockIt duration
             tell [UnlockEvent now]
             tell [LockEvent expire]

        unlockWontExpire :: Integer -> TestController d ()
        unlockWontExpire duration =
          do (now, _) <- unlockIt duration
             tell [UnlockEvent now]

        unlockExtend :: Integer -> TestController d ()
        unlockExtend duration =
          do (_, expire) <- unlockIt duration
             tell [LockEvent expire]

        unlockWillBeIgnored :: Integer -> TestController d ()
        unlockWillBeIgnored duration =
          do _ <- unlockIt duration
             return ()

type CheckedResults = Either ((MockLockEvent, MockLockEvent), Text) Text

checkResults :: [MockLockEvent]
             -> [MockLockEvent]
             -> NominalDiffTime
             -> CheckedResults
checkResults expected actual epsilon = foldr compareResult (Right "No results to compare") $ zip expected actual
  where compareResult :: (MockLockEvent, MockLockEvent) -> CheckedResults -> CheckedResults
        compareResult _ (Left l) = Left l
        compareResult ev@(UnlockEvent t1, UnlockEvent t2) _ =
          if t2 `diffUTCTime` t1 < epsilon
             then Right "OK"
             else Left (ev, "Time difference exceeds epsilon")
        compareResult ev@(LockEvent t1, LockEvent t2) _ =
          if t2 `diffUTCTime` t1 < epsilon
             then Right "OK"
             else Left (ev, "Time difference exceeds epsilon")
        compareResult ev _ = Left (ev, "Event types don't match")

controllerTest :: IO CheckedResults
controllerTest =
  do ml <- mockLock
     cc <- controller (Just 1) $ mockLockDevice ml
     ccEvents <- testController cc
     -- Discard the first MockLock event, which happened when
     -- controller initialized the lock.
     _:lockEvents <- events ml
     return $ checkResults ccEvents lockEvents (0.5 :: NominalDiffTime)

data ExceptionLock =
  ExceptionLock {_ops :: !(MVar Int)
                ,_opsPerException :: !Int}

data ExceptionLockException =
  LockException
  deriving (Show,Typeable)

instance Exception ExceptionLockException

exceptionLock :: Int -> IO ExceptionLock
exceptionLock n =
  do mvar <- newMVar 0
     return $ ExceptionLock mvar n

-- | This device throws an exception every N operations.
exceptionLockDevice :: ExceptionLock -> Device ExceptionLock
exceptionLockDevice l =
  Device inc
         inc
  where
    inc =
      do ops <- modifyMVar (_ops l) $ \n -> return (succ n, succ n)
         when (ops `mod` (_opsPerException l) == 0) $
           throwIO LockException

isExceptionLockException :: ExceptionLockException -> Bool
isExceptionLockException = const True

asyncExceptionTest :: IO ()
asyncExceptionTest =
  do el <- exceptionLock 3
     cc <- controller Nothing $ exceptionLockDevice el -- 1st lock op
     now <- getCurrentTime
     let expire = timePlusN now 3
     void $ unlockController expire cc -- 2nd & 3rd lock op (unlock, timed lock)
     (sleep 5) `shouldThrow` isExceptionLockException -- async exception
     queryController cc `shouldReturn` StateUnlocked expire -- should have state prior to exception

syncExceptionTest :: IO ()
syncExceptionTest =
  do el <- exceptionLock 2
     cc <- controller Nothing $ exceptionLockDevice el -- 1st lock op
     now <- getCurrentTime
     let expire = timePlusN now 3
     unlockController expire cc `shouldThrow` isExceptionLockException -- 2nd lock op
     queryController cc `shouldReturn` StateLocked -- should have state prior to exception

pastUnlockTimeTest :: IO ()
pastUnlockTimeTest =
  do ml <- mockLock
     cc <- controller Nothing $ mockLockDevice ml
     race
       (sleep 3) -- 3 sec should be more than enough time
       (do now <- getCurrentTime
           let past = timePlusN now (-1)
           unlockController past cc)
       `shouldReturn` Right StateLocked

ignoreUnlockTimeTest :: IO ()
ignoreUnlockTimeTest =
  do ml <- mockLock
     cc <- controller (Just 3) $ mockLockDevice ml
     race
       (sleep 3) -- 3 sec should be more than enough time
       (do now <- getCurrentTime
           let expire = timePlusN now 2
           unlockController expire cc)
       `shouldReturn` Right StateLocked

minUnlockTimeTest :: IO ()
minUnlockTimeTest =
  do ml <- mockLock
     cc1 <- controller Nothing $ mockLockDevice ml
     minUnlockTime cc1 `shouldBe` 0
     cc2 <- controller (Just (-1)) $ mockLockDevice ml
     minUnlockTime cc2 `shouldBe` 0

spec :: Spec
spec = do
  describe "Controller tests" $ do
    it "should produce the correct lock sequence plus or minus a few hundred milliseconds" $ do
       -- This test is too timing-sensitive to run on a CI server, so
       -- it's disabled by default.
#ifdef ENABLE_TIMING_SENSITIVE_TESTS
      controllerTest >>= (`shouldBe` Right "OK")
#else
      pendingWith "disabled (enable with 'cabal configure -f enable-timing-sensitive-tests')"
#endif
    it "should recover from asynchronous exceptions" $ do
      asyncExceptionTest
    it "should recover from synchronous exceptions" $ do
      syncExceptionTest
    it "should not wait forever if the unlock time is in the past" $ do
      pastUnlockTimeTest
    it "should ignore an unlock if its duration is less than the minimum unlock time" $ do
      ignoreUnlockTimeTest
    it "should set the minimum unlock time to 0 if the value is Nothing or < 0" $ do
      minUnlockTimeTest
