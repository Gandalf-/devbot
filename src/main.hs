module Main where

import           System.Environment (getArgs)
import           System.Exit        (die)

import           Devbot.Bot
import           Devbot.List
import           Devbot.Load
import           Devbot.Status


main :: IO ()
main = do
        option <- getArgs
        case option of
            ["start"]      -> runBot
            ["list"]       -> runList
            ["status"]     -> runStatus
            ["load", path] -> runLoadConfig path
            ["load"]       -> defaultConfigPath >>= runLoadConfig
            _              -> die usage


usage :: String
usage = "usage: (start | list | status | load [path])"
