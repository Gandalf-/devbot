{-# LANGUAGE LambdaCase #-}

module Devbot.Run (
    execute,
    delay,
    clear)
where

import           System.Exit             (die)

import           Devbot.Event.Config
import           Devbot.Event.Runtime
import           Devbot.Internal.Parser  (parseTime)
import           Devbot.Internal.Persist

delay :: [String] -> IO ()
delay []  = delayUsage
delay [_] = delayUsage
delay (name:interval) = do
        e <- search name
        maybe (die help) (delayUpdate e) time
    where
        delayUpdate :: Event -> Integer -> IO ()
        delayUpdate e t = do
            flush defaultContext $ delayBy e t
            requestRefresh

        time :: Maybe Integer
        time = parseTime $ unwords interval

        help :: String
        help = unlines [
              "could not parse the interval provided"
            , ""
            , "    try '1 hour' or '45 minutes'"
            ]

execute :: String -> IO ()
-- set the next run for this event to now, executing it as soon as possible. the
-- this request may be lost if it's clobbered by the bot
execute name = do
        search name >>= flush defaultContext . runNow
        requestRefresh

clear :: String -> IO ()
-- clear errors and set the next time for this event to run to now
-- this request may be lost if it's clobbered by the bot
clear name = do
        search name >>= flush defaultContext . runNow . clearErrors
        requestRefresh


clearErrors :: Event -> Event
clearErrors e = e { _data = (_data e) { _errors = Nothing }}

runNow :: Event -> Event
runNow e = e { _data = (_data e) { _when = 0 }}

delayBy :: Event -> Integer -> Event
-- set the next time for this event to what it was plus the value provided
delayBy e skew = e { _data = (_data e) { _when = new }}
    where
        new = old + skew
        old = _when $ _data e

search :: String -> IO Event
search name =
        filter (\e -> name == _name e) <$> events >>= \case
            []  -> die $ "Cannot find an event named " <> name
            [e] -> pure e
            _   -> die $ name <> " unexpectedly matched multiple events"

requestRefresh :: IO ()
requestRefresh = do
        cx <- defaultContext
        set cx ["devbot", "reload"] True

delayUsage :: IO ()
delayUsage = die "<event> <interval...>"
