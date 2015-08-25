{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Concurrent as CC (threadDelay)
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import qualified Data.Time as Time (getCurrentTime)
import Options.Applicative
import Prelude hiding (putStrLn)
import qualified Prelude as Prelude (putStrLn)
import System.Mellon.Controller (MonadController(..), ConcurrentControllerT(..), concurrentController, runConcurrentControllerT)
import System.Mellon.Lock (MonadLock(..), MockLockEvent(..), events, mockLock, runMockLockT)
import System.Mellon.StateMachine (State(..))

data Verbosity
  = Normal
  | Verbose

data GlobalOptions =
  GlobalOptions {quiet :: Bool
                ,verbose :: Verbosity
                ,cmd :: Command}

data Command
  = Concurrent ConcurrentOptions

data ConcurrentOptions = ConcurrentOptions {unusedConcurrent :: Maybe String}

concurrentCmd :: Parser Command
concurrentCmd = Concurrent <$> concurrentOptions

concurrentOptions :: Parser ConcurrentOptions
concurrentOptions =
  ConcurrentOptions <$>
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
    (command "concurrent" (info concurrentCmd (progDesc "Run the concurrent controller test")))

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

type TestConcurrent m a = WriterT [MockLockEvent] (ConcurrentControllerT m) a

testCC :: (MonadLock m, MonadIO m) => ConcurrentControllerT m [MockLockEvent]
testCC =
  do expectedResults <- execWriterT theTest
     return expectedResults

  where theTest :: (MonadIO m, MonadLock m) => TestConcurrent m ()
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

        lockIt :: (MonadIO m, MonadLock m) => TestConcurrent m ()
        lockIt =
          do now <- getCurrentTime
             lockNow
             tell [LockEvent now]

        unlock_ :: (MonadIO m, MonadLock m) => Integer -> TestConcurrent m (UTCTime, UTCTime)
        unlock_ duration =
          do now <- getCurrentTime
             let expire = timePlusN now duration
             unlockUntil expire
             return (now, expire)

        unlockWillExpire :: (MonadIO m, MonadLock m) => Integer -> TestConcurrent m ()
        unlockWillExpire duration =
          do (now, expire) <- unlock_ duration
             tell [UnlockEvent now]
             tell [LockEvent expire]

        unlockWontExpire :: (MonadIO m, MonadLock m) => Integer -> TestConcurrent m ()
        unlockWontExpire duration =
          do (now, _) <- unlock_ duration
             tell [UnlockEvent now]

        unlockWillBeIgnored :: (MonadIO m, MonadLock m) => Integer -> TestConcurrent m ()
        unlockWillBeIgnored duration =
          do _ <- unlock_ duration
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

run :: GlobalOptions -> IO ()
run (GlobalOptions False _ (Concurrent _)) =
  do cc <- concurrentController Locked
     ml <- mockLock
     ccEvents <- runMockLockT ml $ runConcurrentControllerT cc testCC
     lockEvents <- events ml
     let outcome = checkResults ccEvents lockEvents (0.5 :: NominalDiffTime)
     print outcome
     return ()
run _ = return ()

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Mellon electric strike controller" <>
                header "mellon - a command-based CLI for mellon")
