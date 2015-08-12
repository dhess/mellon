{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Concurrent as CC (forkIO, threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import qualified Data.Time as Time (getCurrentTime)
import Options.Applicative
import Prelude hiding (putStrLn)
import qualified Prelude as Prelude (putStrLn)
import System.Mellon.Controller (ConcurrentController, ControllerT, concurrentController, runConcurrentControllerT, runConcurrentStateMachine, unlockUntil, lockNow)
import System.Mellon.Lock (MonadLock(..), MockLockEvent(..), MockLockT, execMockLockT, runMockLockT)

data Verbosity
  = Normal
  | Verbose

data GlobalOptions =
  GlobalOptions {quiet :: Bool
                ,verbose :: Verbosity
                ,cmd :: Command}

data Command
  = Concurrent ConcurrentOptions
  | MockLockCmd MockLockOptions

data ConcurrentOptions = ConcurrentOptions {unusedConcurrent :: Maybe String}

concurrentCmd :: Parser Command
concurrentCmd = Concurrent <$> concurrentOptions

concurrentOptions :: Parser ConcurrentOptions
concurrentOptions =
  ConcurrentOptions <$>
  optional (strOption (help "unused"))

data MockLockOptions = MockLockOptions {unusedMockLock :: Maybe String}

mockLockCmd :: Parser Command
mockLockCmd = MockLockCmd <$> mockLockOptions

mockLockOptions :: Parser MockLockOptions
mockLockOptions =
  MockLockOptions <$>
  optional (strOption (help "unused"))

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
  switch (long "quiet" <>
          short 'q' <>
          help "Be quiet") <*>
  flag Normal
       Verbose
       (long "verbose" <>
        short 'v' <>
        help "Enable verbose mode") <*>
  hsubparser
    (command "concurrent" (info concurrentCmd (progDesc "Run the concurrent controller test")) <>
     command "mocklock" (info mockLockCmd (progDesc "Run the mock lock test")))

sleep :: MonadIO m => Int -> m ()
sleep = liftIO . CC.threadDelay . (* 1000000)

threadDelay :: MonadIO m => Int -> m ()
threadDelay = liftIO . CC.threadDelay

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Time.getCurrentTime

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . Prelude.putStrLn

timePlusN :: UTCTime -> Integer -> UTCTime
timePlusN time n = (fromInteger n) `addUTCTime` time

type TestConcurrent a = WriterT [MockLockEvent] (ControllerT IO) a

testCC :: ControllerT IO [MockLockEvent]
testCC =
  do expectedResults <- execWriterT theTest
     return expectedResults

  where theTest :: TestConcurrent ()
        theTest =
          do putStrLn "Beginning test. This will take about 1 minute."
             unlockWillExpire 5
             sleep 8
             unlockWontExpire 3
             sleep 1
             unlockWillExpire 10
             sleep 14
             unlockWillExpire 8
             sleep 2
             unlockWillBeIgnored 1
             sleep 13
             unlockWontExpire 8
             sleep 3
             lockIt
             sleep 12

        lockIt :: TestConcurrent ()
        lockIt =
          do now <- getCurrentTime
             lockNow
             tell [LockEvent now]

        unlock_ :: Integer -> TestConcurrent (UTCTime, UTCTime)
        unlock_ duration =
          do now <- getCurrentTime
             let expire = timePlusN now duration
             unlockUntil expire
             return (now, expire)

        unlockWillExpire :: Integer -> TestConcurrent ()
        unlockWillExpire duration =
          do (now, expire) <- unlock_ duration
             tell [UnlockEvent now]
             tell [LockEvent expire]

        unlockWontExpire :: Integer -> TestConcurrent ()
        unlockWontExpire duration =
          do (now, _) <- unlock_ duration
             tell [UnlockEvent now]

        unlockWillBeIgnored :: Integer -> TestConcurrent ()
        unlockWillBeIgnored duration =
          do _ <- unlock_ duration
             return ()

runCCTest :: ConcurrentController [MockLockEvent] -> IO ()
runCCTest cc =
  do _ <- runConcurrentControllerT cc testCC
     return ()

type CheckedResults = Either ((MockLockEvent, MockLockEvent), String) String

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

testMockLock :: MockLockT IO ()
testMockLock =
  do lock
     unlock
     unlock
     lock

run :: GlobalOptions -> IO ()
run (GlobalOptions False _ (Concurrent _)) =
  do cc :: ConcurrentController [MockLockEvent] <- concurrentController
     --_ <- CC.forkIO (evalMockLockT $ runConcurrentStateMachine cc ())
     --runConcurrentControllerT cc testConcurrent
     _ <- CC.forkIO (runCCTest cc)
     (ccEvents, lockEvents) <- runMockLockT $ runConcurrentStateMachine cc
     let outcome = checkResults ccEvents lockEvents (0.5 :: NominalDiffTime)
     print outcome

run (GlobalOptions False _ (MockLockCmd _)) =
  do output <- execMockLockT testMockLock
     print output
run _ = return ()

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Mellon electric strike controller" <>
                header "mellon - a command-based CLI for mellon")
