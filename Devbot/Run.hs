module Devbot.Run (
    execute,
    clear)
where

import           System.Exit             (die)

import           Devbot.Event.Config
import           Devbot.Event.Runtime
import           Devbot.Internal.Persist


execute :: String -> IO ()
-- set the next run for this event to now, executing it as soon as possible. the
-- this request may be lost if it's clobbered by the bot
execute name = do
        e <- search name
        flush defaultContext $ runNow e
        requestRefresh

clear :: String -> IO ()
-- clear errors and set the next time for this event to run to now
-- this request may be lost if it's clobbered by the bot
clear name = do
        e <- search name
        flush defaultContext $ runNow $ clearErrors e
        requestRefresh


clearErrors :: Event -> Event
clearErrors e = e { _data = (_data e) { _errors = Nothing }}

runNow :: Event -> Event
runNow e = e { _data = (_data e) { _when = 0 }}

search :: String -> IO Event
search name =
        filter (\e -> name == _name e) <$> events >>= \case
            []  -> die $ "Cannot find an event named " <> name
            [e] -> pure e
            _   -> die $ name <> " matched multiple events"

requestRefresh :: IO ()
requestRefresh = do
        cx <- defaultContext
        set cx ["devbot", "reload"] True
