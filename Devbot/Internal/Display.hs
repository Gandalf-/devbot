module Devbot.Internal.Display where

import           Devbot.Internal.ColorText
import           Devbot.Internal.Parser

data Iteration = First | Second | Third

granularTime :: Integer -> String
granularTime = pTime First

prettyTime :: Integer -> String
prettyTime = pTime Second

pTime :: Iteration -> Integer -> String
-- ^ pretty time that does all the work. show the two highest denominations of
-- time for the given number of seconds
--
-- 121  = "2 minutes, 1 second"
-- 3661 = "1 hour, 1 minute"     -- note that seconds are omitted
pTime Third _ = ""
pTime b i
        | i == 0     = ""
        | i < minute = showTime i "second"              <> after
        | i < hour   = showTime (div i minute) "minute" <> after <> pTime (next b) (mod i minute)
        | i < day    = showTime (div i hour)   "hour"   <> after <> pTime (next b) (mod i hour)
        | i < week   = showTime (div i day)    "day"    <> after <> pTime (next b) (mod i day)
        | i < month  = showTime (div i week)   "week"   <> after <> pTime (next b) (mod i week)
        | i < year   = showTime (div i month)  "month"  <> after <> pTime (next b) (mod i month)
        | otherwise  = showTime (div i year)   "year"   <> after <> pTime (next b) (mod i year)
    where
        after :: String
        after = case b of
            First -> ", "
            _     -> ""

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

white :: Decoration
white = (White,   NoColor, Null)
