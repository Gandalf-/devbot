module Devbot.Table (
    runTable
) where

import           Data.Char                 (isSpace)
import           Data.List                 (intercalate, sort, transpose)
import           Data.Maybe                (fromMaybe)

import           Devbot.Internal.ColorText (Decoration)
import           Devbot.Internal.Common
import           Devbot.Internal.Display


import           Devbot.Event.Config       (Event)
import qualified Devbot.Event.Config       as E

import           Devbot.Service.Config     (Service)
import qualified Devbot.Service.Config     as S

type Uptime = Maybe Integer
type CurrentTime = Integer


runTable :: IO ()
-- ^ build two tables to show all the runtime and configuration information relevant to
-- the events and services that are currently loaded. this reads out of the devbot
-- database, not the config file, so it reflects what a running devbot sees as the
-- "state of the world" right now
runTable = do
    now <- fromInteger <$> getTime :: IO CurrentTime

    putStrLn ""
    E.events >>= (mapM_ putStrLn . eventTable now)
    putStr "\n\n"

    -- we pair uptime and service info so all the IO is isolated to this function
    ss <- S.services
    uptimes <- mapM (S.getUptime . S._name) ss
    mapM_ putStrLn $ serviceTable now (zip ss uptimes)
    putStrLn ""


-- events

eventTable :: CurrentTime -> [Event] -> [String]
-- ^ build the table data for the events, we start with columns, so we can use
-- smartbuffer to make them the same width. after that, we filter out empty columns,
-- colorize, and then rotate. the rotation makes the columns into rows, where now the
-- rows correspond to all the values for a particular event
eventTable now es =
        reverse $ map (intercalate "|") $ rotate $ colorColumns columns
    where
        columns = filter (not . columnEmpty) $ zip
            [green, blue, yellow, cyan, red, magenta, magenta]
            $ map (smartBuffer Center) elements

        elements =
            [ ["events",  ""] <> map getEventName ses
            , ["every",   ""] <> map getEventInterval ses
            , ["next",    ""] <> map (getEventNext now) ses
            , ["last",    ""] <> map getEventLast ses
            , ["errors",  ""] <> map getEventErrors ses
            , ["require", ""] <> map getEventRequires ses
            , ["options", ""] <> map getEventOptions ses
            ]

        ses = sort es

-- | event helpers

getEventName :: Event -> String
getEventName = E._name

getEventInterval :: Event -> String
getEventInterval = clean . prettyTime . E.interval . E._config

getEventNext :: CurrentTime -> Event -> String
getEventNext now e =
        if next > 0
            then prettyTime next
            else "now"
    where
        next = E._when (E._data e) - now

getEventLast :: Event -> String
getEventLast e
        | null time = "instant"
        | otherwise = time
    where
        time = prettyTime . E._duration $ E._data e

getEventRequires :: Event -> String
getEventRequires = fromMaybe "" . E.require . E._config

getEventErrors :: Event -> String
getEventErrors e =
        case count of
            Nothing  -> ""
            (Just c) -> show c
    where
        count = E._errors $ E._data e

getEventOptions :: Event -> String
getEventOptions e = unwords [parallel, oneshell]
    where
        parallel = if E.parallel $ E._config e then "P" else ""
        oneshell = if E.oneshell $ E._config e then "" else "O"


-- | services

serviceTable :: CurrentTime -> [(Service, Uptime)] -> [String]
-- ^ services are dealt with indentically as events, except that there are different
-- headers and values used for the columns
serviceTable now ss =
        reverse $ map (intercalate "|") $ rotate $ colorColumns columns
    where
        columns = filter (not . columnEmpty) $ zip
            [blue, green, cyan]
            $ map (smartBuffer Center) elements

        elements =
            [ ["services", ""] <> map getServiceName sss
            , ["uptime",   ""] <> map (getServiceUptime now) sss
            , ["action",   ""] <> map getServiceAction sss
            ]

        sss = sort ss

-- | service helpers

getServiceName :: (Service, Uptime) -> String
getServiceName = S._name . fst

getServiceUptime :: Integer -> (Service, Uptime) -> String
getServiceUptime now (_, uptime) =
        case uptime of
            Nothing  -> "dead"
            (Just u) -> prettyTime $ now - u

getServiceAction :: (Service, Uptime) -> String
getServiceAction = S.action . S._config . fst


-- | shared utilities

data Alignment = Righty | Center

buffer :: Alignment -> Int -> String -> String
-- ^ pad the given string to a particular length, using an alignment scheme
buffer Righty n s = take n $ s <> cycle " "
buffer Center n s = take n body
    where
        body = pad <> s <> cycle " "
        pad = replicate ((n - length s) `div` 2) ' '

smartBuffer :: Alignment -> [String] -> [String]
-- ^ choose an appropriate buffer pad size given a list of elements so that none are
-- truncated and to limit extra output space
smartBuffer a column = map (buffer a pad) column
    where
        pad = (+ 4) $ maximum $ map length column

colorColumns :: [(Decoration, [String])] -> [[String]]
-- ^ apply the color to all elements for each color + elements pair
colorColumns =
        map (\(color, xs) -> map (colorize color) xs)

rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

columnEmpty :: (Decoration, [String]) -> Bool
-- ^ does some element after the first have more content than just spaces? we skip the
-- first element because it's a header and will always be present
columnEmpty (_, xs) = all (all isSpace) $ tail xs
