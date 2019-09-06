{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment        (getArgs)
import           System.Exit               (die)

import           Devbot.Bot
import           Devbot.Daemon
import           Devbot.List
import           Devbot.Schema
import           Devbot.Status

import           Devbot.Internal.Directory
import           Devbot.Internal.System


main :: IO ()
main = do
        ensureDefaultDirectory

        getArgs >>= \case
            ["start"]  -> ifRunningElse
                (exit "running")
                (saveDevbotPid >> runBot)

            ["daemon"] -> ifRunningElse
                (exit "running")
                runDaemon

            ["stop"]   -> ifRunningElse
                stopDaemon
                (exit "stopped")

            ["list"]   -> runList
            ["status"] -> runStatus
            ["schema"] -> runSchema

            ["config"] -> getConfigPath >>= putStrLn

            -- help text
            _          -> die usage
    where
        exit :: String -> IO ()
        exit x = die $ "devbot appears to already be " <> x

        ifRunningElse :: IO b -> IO b -> IO b
        ifRunningElse a b = getStatus >>= \case
            Running -> a
            _       -> b


usage :: String
usage = unlines
    [ "devbot usage: "
    , "  start       - start devbot"
    , "  daemon      - start devbot in the background"
    , "  stop        - stop devbot and all services"
    , ""
    , "  list        - show a summary of runtime data and config"
    , "  status      - give a single character summary of run state"
    , ""
    , "  schema      - show the config file schema"
    , "  config      - show the config file path"
    ]
