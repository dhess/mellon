-- | This is a one-shot program which, when run, checks the specified
-- unlock window against the current time. If the current time is
-- within the window, the program unlocks the specified Mellon
-- controller until the end of the window. If the current time is
-- outside the window, the program locks the controller.
--
-- The program is intended to be run from a cron job which runs once a
-- day at the start of the window, therefore implementing a very
-- simple, "unlock this door every day from time X until time Y"
-- system. Because it checks the current time against the unlock
-- window, it can also be run at intervals (e.g., once a minute) or
-- @reboot to make the system slightly more robust.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude hiding (State, state)
import Control.Monad.Catch.Pure (runCatch)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Time.Clock
       (NominalDiffTime, UTCTime(..), addUTCTime)
import qualified Data.Time.LocalTime as Time
       (LocalTime(..), TimeOfDay(..), ZonedTime(..), getZonedTime,
        zonedTimeToUTC)
import Mellon.Web.Client (State(..), putState)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status(..))
import Options.Applicative
import Servant.Client
       (BaseUrl, ClientEnv(..), ServantError(..), parseBaseUrl,
        runClientM)
import System.Exit (ExitCode(..), exitWith)

data GlobalOptions =
  GlobalOptions {_url :: !BaseUrl
                ,_cmd :: !Command}

data Command
  = LocalTime LocalTimeOptions

data LocalTimeOptions =
  LocalTimeOptions {_localTimeStart :: !Time.TimeOfDay
                   ,_localTimeEnd :: !Time.TimeOfDay}

localTimeCmd :: Parser Command
localTimeCmd = LocalTime <$> localTimeOptions

localTimeOptions :: Parser LocalTimeOptions
localTimeOptions =
  LocalTimeOptions <$>
  argument auto (metavar "HH:MM:SS" <>
                 help "Unlock window start time (local time, 24H format)") <*>
  argument auto (metavar "HH:MM:SS" <>
                 help "Unlock window end time (local time, 24H format)")

parseServiceUrl :: String -> ReadM BaseUrl
parseServiceUrl s =
  case runCatch $ parseBaseUrl s of
    Left _ -> readerError $ "Invalid service URL: " ++ s
    Right url -> return url

cmds :: Parser GlobalOptions
cmds =
  GlobalOptions <$>
  argument (str >>= parseServiceUrl)
           (metavar "URL" <>
            help "Mellon server base URL") <*>
  hsubparser
    (command "localtime" (info localTimeCmd (progDesc "Unlock window specified in local time")))

oneDay :: NominalDiffTime
oneDay = 60 * 60 * 24

-- | Convert a 'TimeOfDay' to a 'UTCTime', using the given 'ZonedTime'
-- as the point of reference; i.e., the 'TimeOfDay' is relative to the
-- 'ZonedTime''s (local) day.
timeOfDayToUTC :: Time.TimeOfDay -> Time.ZonedTime -> UTCTime
timeOfDayToUTC tod zt =
  let localDay = Time.localDay $ Time.zonedTimeToLocalTime zt
      localTz = Time.zonedTimeZone zt
      localTime = Time.LocalTime localDay tod
  in Time.zonedTimeToUTC $ Time.ZonedTime localTime localTz

run :: GlobalOptions -> IO ()
run (GlobalOptions baseUrl (LocalTime (LocalTimeOptions localStart localEnd))) =
  do zonedNow <- Time.getZonedTime
     let utcStart = timeOfDayToUTC localStart zonedNow
         adjustEnd =
           if localStart > localEnd
              then oneDay
              else 0
         utcEnd = addUTCTime adjustEnd $ timeOfDayToUTC localEnd zonedNow
         utcNow = Time.zonedTimeToUTC zonedNow
     go utcStart utcEnd utcNow baseUrl
  where
    go :: UTCTime -> UTCTime -> UTCTime -> BaseUrl -> IO ()
    go start end now url =
      let state = if now >= start && now < end
                     then Unlocked end
                     else Locked
      in do manager <- newManager tlsManagerSettings
            let clientEnv = ClientEnv manager url
            runClientM (putState state) clientEnv >>= \case
                        Right status ->
                          do putStrLn $ ((show status) :: String)
                             exitWith ExitSuccess
                        Left e ->
                          do putStrLn $ "Mellon service error: " ++ prettyServantError e
                             exitWith $ ExitFailure 1
    prettyServantError :: ServantError -> String
    prettyServantError (FailureResponse _ status _ _) =
      show (statusCode status) ++ " " ++ (toS $ statusMessage status)
    prettyServantError (DecodeFailure _ _ _) =
      "decode failure"
    prettyServantError (UnsupportedContentType _ _) =
      "unsupported content type"
    prettyServantError (InvalidContentTypeHeader _ _) =
      "invalid content type header"
    prettyServantError (ConnectionError _) =
      "connection refused"

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Unlock a mellon controller within a specified time window" <>
                header "mellon-schedule-unlock")
