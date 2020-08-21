{-# LANGUAGE TupleSections #-}

module Devbot.Table (
    runTable
) where

import           Control.Monad
import           Data.Char                 (isSpace)
import           Data.List                 (intercalate, sort, transpose)
import           Data.Maybe                (mapMaybe, isJust)

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

    -- we pair uptime and service info so all the IO is isolated to this function
    ss <- S.services
    unless (null ss) $
        putStrLn ""

    uptimes <- mapM (S.getUptime . S._name) ss
    mapM_ putStrLn $ serviceTable now (zip ss uptimes)
    putStrLn ""


-- | events

eventTable :: CurrentTime -> [Event] -> [String]
-- ^ build the table data for the events, we start with columns, so we can use
-- smartbuffer to make them the same width. after that, we filter out empty columns,
-- colorize, and then rotate. the rotation makes the columns into rows, where now the
-- rows correspond to all the values for a particular event
eventTable now es =
        reverse $ map (intercalate "|") $ rotate $ mapMaybe (applyBuffer Center) elements
    where
        elements :: [[(Decoration, String)]]
        elements =
            [ map (green,)   ["events",  ""] <> map getEventName       ses
            , map (blue,)    ["every",   ""] <> map getEventInterval   ses
            , map (yellow,)  ["next",    ""] <> map (getEventNext now) ses
            , map (cyan,)    ["last",    ""] <> map getEventLast       ses
            , map (red,)     ["errors",  ""] <> map getEventErrors     ses
            , map (magenta,) ["options", ""] <> map getEventOptions    ses
            ]

        ses = sort es

        applyBuffer :: Alignment -> [(Decoration, String)] -> Maybe [String]
        applyBuffer a ds
                | all (all isSpace) $ tail buffered = Nothing
                | otherwise                         = Just result
            where
                result   = zipWith colorize colors buffered

                colors   = map fst ds
                buffered = smartBuffer a $ map snd ds

-- | event helpers

getEventName :: Event -> (Decoration, String)
getEventName = (,) green . E._name

getEventInterval :: Event -> (Decoration, String)
getEventInterval = (,) blue . clean . prettyTime . E.interval . E._config

getEventNext :: CurrentTime -> Event -> (Decoration, String)
getEventNext now e
        | next > 0  = (yellow, prettyTime next)
        | otherwise = (white, "now")
    where
        next = E._when (E._data e) - now

getEventLast :: Event -> (Decoration, String)
getEventLast e
        | time == 0       = (cyan, "instant")
        | time < interval = (cyan, prettyTime time)
        | otherwise       = (red, prettyTime time)
    where
        time = E._duration $ E._data e
        interval = E.interval $ E._config e

getEventErrors :: Event -> (Decoration, String)
getEventErrors e =
        (,) red $ case count of
            Nothing  -> ""
            (Just c) -> show c
    where
        count = E._errors $ E._data e

getEventOptions :: Event -> (Decoration, String)
getEventOptions e = (magenta, unwords [require, monitor, parallel, oneshell])
    where
        parallel = if E.parallel $ E._config e then "P" else ""
        oneshell = if E.oneshell $ E._config e then "" else "O"
        require  = if isJust (E.require $ E._config e) then "R" else ""
        monitor  = if isJust (E.monitor $ E._config e) then "M" else ""


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
        maybe "dead" display uptime
    where
        display t
                | time < 60 * 60 = prettyTime time
                | otherwise      = granularTime time
            where
                time = now - t

getServiceAction :: (Service, Uptime) -> String
getServiceAction = S.action . S._config . fst


-- | shared utilities

data Alignment = Lefty | Center

buffer :: Alignment -> Int -> String -> String
-- ^ pad the given string to a particular length, using an alignment scheme
buffer Lefty  n s = take n $ s <> cycle " "
buffer Center n s = take n body
    where
        body = pad <> s <> cycle " "
        pad = replicate ((n - length s) `div` 2) ' '

smartBuffer :: Alignment -> [String] -> [String]
-- ^ choose an appropriate buffer pad size given a list of elements so that none are
-- truncated and to limit extra output space
smartBuffer a column = map (buffer a pad) column
    where
        pad = (+ 2) $ maximum $ map length column

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
