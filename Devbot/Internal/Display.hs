module Devbot.Internal.Display where

import           Devbot.Internal.ColorText
import           Devbot.Internal.Parser

data Iteration = First | Second | Third

prettyTime :: Integer -> String
prettyTime = pTime First

pTime :: Iteration -> Integer -> String
-- ^ pretty time that does all the work. show the two highest denominations of
-- time for the given number of seconds
--
-- 121  = "2 minutes, 1 second"
-- 3661 = "1 hour, 1 minute"     -- note that seconds are omitted
pTime Second _ = ""
pTime b i
        | i == 0     = ""
        | i < minute = before <> showTime i "second"
        | i < hour   = before <> showTime (div i minute) "minute" <> pTime (next b) (mod i minute)
        | i < day    = before <> showTime (div i hour)   "hour"   <> pTime (next b) (mod i hour)
        | i < week   = before <> showTime (div i day)    "day"    <> pTime (next b) (mod i day)
        | i < month  = before <> showTime (div i week)   "week"   <> pTime (next b) (mod i week)
        | i < year   = before <> showTime (div i month)  "month"  <> pTime (next b) (mod i month)
        | otherwise  = before <> showTime (div i year)   "year"   <> pTime (next b) (mod i year)
    where
        before :: String
        before = case b of
            First -> ""
            _     -> ", "

        showTime :: Integer -> String -> String
        showTime 1 s = "1 " <> s
        showTime x s = show x <> " " <> s <> "s"

        next :: Iteration -> Iteration
        next First  = Second
        next Second = Third
        next Third  = Third

clean :: String -> String
clean ('1' : ' ' : xs) = xs
clean xs               = xs


colorize :: Decoration -> String -> String
colorize = flip decorate

blue :: Decoration
blue = (Blue,   NoColor, Null)

green :: Decoration
green = (Green,  NoColor, Bold)

yellow :: Decoration
yellow = (Yellow, NoColor, Null)

red :: Decoration
red = (Red,    NoColor, Null)

cyan :: Decoration
cyan = (Cyan,   NoColor, Null)

magenta :: Decoration
magenta = (Magenta,   NoColor, Null)
