module Devbot.Internal.Parser (
    parseTime, readNumber,
    minute, hour, day, week, month, year
) where

import           Text.Read (readMaybe)

parseTime :: String -> Maybe Integer
parseTime = parse . words

-- | Attempt to parse natural language into an equivalent number of seconds
parse :: [String] -> Maybe Integer

parse ["hourly"]  = Just hour
parse ["daily"]   = Just day
parse ["weekly"]  = Just week
parse ["monthly"] = Just month

parse ["second"]  = Just 1
parse ["minute"]  = Just minute
parse ["hour"]    = Just hour
parse ["day"]     = Just day
parse ["week"]    = Just week
parse ["month"]   = Just month
parse ["year"]    = Just year

parse ["seconds"] = Just 1
parse ["minutes"] = Just minute
parse ["hours"]   = Just hour
parse ["days"]    = Just day
parse ["weeks"]   = Just week
parse ["months"]  = Just month
parse ["years"]   = Just year

parse ("twice":"per":x) = flip div 2 <$> parse x

parse ("twice":x) = flip div 2 <$> parse x

parse ("every":"other":y) = (* 2) <$> parse y

parse ["every", x] = parse [x]

parse ("every":x:y) = result
    where
        result = (*) <$> number <*> next
        number = readNumber x
        next   = parse y

parse (x:"times":"per":y) = result
    where
        result = div <$> next <*> number
        number = readNumber x
        next   = parse y

parse [x]           = readNumber x
parse _             = Nothing


-- | Try to be flexible in how we interpret numbers
readNumber :: String -> Maybe Integer

readNumber "one"   = Just 1
readNumber "two"   = Just 2
readNumber "three" = Just 3
readNumber "four"  = Just 4
readNumber "five"  = Just 5
readNumber "six"   = Just 6
readNumber "seven" = Just 7
readNumber "eight" = Just 8
readNumber "nine"  = Just 9
readNumber "ten"   = Just 10

-- assuming that people prefer "18" to "eighteen", we'll stop here

readNumber x       = readMaybe x


-- | Constants
year :: Integer
year = month * 12

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
