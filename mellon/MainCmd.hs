module Main where

import Options.Applicative

data Verbosity
  = Normal
  | Verbose

data GlobalOptions =
  GlobalOptions {quiet :: Bool
                ,verbose :: Verbosity
                ,cmd :: Command}

data Command
  = Hello HelloOptions

data HelloOptions =
  HelloOptions {greeting :: String
               ,target :: String}

helloCmd :: Parser Command
helloCmd = Hello <$> helloOptions

helloOptions :: Parser HelloOptions
helloOptions =
  HelloOptions <$>
  strOption (long "greeting" <>
             short 'g' <>
             value "Hello" <>
             metavar "GREETING" <>
             help "The greeting") <*>
  argument str (metavar "TARGET")

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
    (command "hello" (info helloCmd (progDesc "Say hello")))

run :: GlobalOptions -> IO ()
run (GlobalOptions False _ (Hello (HelloOptions g t))) = putStrLn (g ++ ", " ++ t)
run _ = return ()

main :: IO ()
main = execParser opts >>= run
  where opts =
          info (helper <*> cmds)
               (fullDesc <>
                progDesc "Say hello and goodbye" <>
                header "mellon-cmd - a command-based CLI for mellon")
