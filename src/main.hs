module Main where

import           System.Environment        (getArgs)
import           System.Exit               (die)

import           Devbot.Bot
import           Devbot.Daemon
import           Devbot.List
import           Devbot.ParseCheck
import           Devbot.Run
import           Devbot.Schema
import           Devbot.Status
import           Devbot.Table

import           Devbot.Internal.Directory
import           Devbot.Internal.System


main :: IO ()
main = do
        ensureDefaultDirectory

        getArgs >>= \case
            ["start"]   -> bail "running" `ifRunningElse` (saveDevbotPid >> runBot)
            ["daemon"]  -> bail "running" `ifRunningElse` runDaemon
            ["stop"]    -> stopDaemon     `ifRunningElse` bail "stopped"

            ["list"]    -> runList
            ["table"]   -> runTable
            ["status"]  -> runStatus

            ("clear":xs) -> mapM_ clear xs
            ("run":xs)   -> mapM_ execute xs
            ("delay":xs) -> delay xs

            ["schema"]   -> runSchema
            ["config"]   -> getConfigPath >>= putStrLn
            ("parse":xs) -> runParseCheck xs

            -- help text
            _          -> die usage
    where
        bail :: String -> IO ()
        bail x = die $ "devbot appears to already be " <> x

        ifRunningElse :: IO b -> IO b -> IO b
        ifRunningElse a b = getStatus >>= \case
            Running -> a
            _       -> b


usage :: String
usage = unlines
    [ "devbot usage: "
    , "  start         - start devbot in the foreground"
    , "  daemon        - start devbot in the background"
    , "  stop          - stop devbot and all services"
    , ""
    , "  list          - show a list summary of runtime data and config"
    , "  table         - show a table summary of runtime data and config"
    , "  status        - give a single character summary of runtime state"
    , ""
    , "  run   <event...>"
    , "                - ask for the following events run immediately"
    , "  clear <event...>"
    , "                - ask for the following events to be cleared of errors"
    , "                  and then run immediately"
    , "  delay <event> <interval...>"
    , "                - ask for the event to be delayed by the interval provided"
    , ""
    , "  schema        - show the config file schema"
    , "  config        - show the config file path"
    , "  parse  <expression...>"
    , "                - show how devbot will interpet an interval expression"
    ]
