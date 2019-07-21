module Devbot.Parser (parseTime) where

import           Text.Read           (readMaybe)

parseTime :: String -> Maybe Integer
parseTime = parse . words

-- | Attempt to parse natural language into an equivalent number of seconds
parse :: [String] -> Maybe Integer

parse ["hourly"] = Just hour
parse ["daily"]  = Just day
parse ["weekly"] = Just week
parse ["monthly"] = Just month

parse ["second"] = Just 1
parse ["minute"] = Just minute
parse ["hour"]   = Just hour
parse ["day"]    = Just day
parse ["week"]   = Just week
parse ["month"]  = Just month

parse ["seconds"] = Just 1
parse ["minutes"] = Just minute
parse ["hours"]   = Just hour
parse ["days"]    = Just day
parse ["weeks"]   = Just week
parse ["months"]  = Just month

parse ("twice":x) = flip div 2 <$> parse x

parse ("every":"other":y) = (* 2) <$> parse y

parse ["every", x] = parse [x]

parse ("every":x:y) = result
    where
        result = (*) <$> number <*> next
        number = readMaybe x :: Maybe Integer
        next   = parse y

parse (x:"times":"per":y) = result
    where
        result = div <$> next <*> number
        number = readMaybe x :: Maybe Integer
        next   = parse y

parse _           = Nothing


month :: Integer
month = week * 4

week :: Integer
week = day * 7

day :: Integer
day  = hour * 24

hour :: Integer
hour = minute * 60

minute :: Integer
minute = 60
