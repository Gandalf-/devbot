module Main where

import           System.Environment (getArgs)
import           System.Exit        (die)

import           Devbot.Bot.Core
import           Devbot.List
import           Devbot.Load
import           Devbot.Schema
import           Devbot.Status


main :: IO ()
main = do
        option <- getArgs
        case option of
            ["start"]  -> runBot
            ["list"]   -> runList
            ["status"] -> runStatus
            ["schema"] -> runSchema

            -- help text
            _          -> defaultConfigPath >>= die . usage


usage :: FilePath -> String
usage path = unlines
    [ "devbot usage: "
    , "  start       - start the devbot daemon"
    , ""
    , "  list        - show a summary of runtime data and config"
    , "  status      - give a single character summary of run state"
    , ""
    , "  schema      - show the config file schema"
    , "  load        - load the config file: " <> path
    ]
