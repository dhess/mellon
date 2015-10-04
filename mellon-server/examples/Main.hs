{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp
import Options.Applicative
import Mellon.Controller
import Mellon.Lock.Mock
import Mellon.Server (app)

data Verbosity
  = Normal
  | Verbose

data GlobalOptions =
  GlobalOptions {quiet :: Bool
                ,verbose :: Verbosity
                ,cmd :: Command}

data Command
  = Serve ServeOptions

data ServeOptions =
  ServeOptions {_port :: Int}

serveCmd :: Parser Command
serveCmd = Serve <$> serveOptions

serveOptions :: Parser ServeOptions
serveOptions =
  ServeOptions <$>
  option auto (long "port" <>
               short 'p' <>
               value 8081 <>
               metavar "PORT" <>
               help "Server port number")

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
    (command "serve" (info serveCmd (progDesc "Start the service")))

runCmd :: GlobalOptions -> IO ()
runCmd (GlobalOptions False _ (Serve (ServeOptions port))) = serve port
runCmd _ = return ()

serve :: Int -> IO ()
serve port =
  do ml <- mockLock
     cc <- concurrentController ml
     run port $ app cc

main :: IO ()
main = execParser opts >>= runCmd
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "A web service for mellon controllers" <>
                header "mellon-server")
