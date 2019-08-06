module Devbot.List
    ( runList
    ) where

import           Data.List         (intercalate, sortOn)
import           Data.Maybe        (fromMaybe)

import           Devbot.Bot.Common
import           Devbot.Parser

import           Devbot.Event      (Config (..), Data (..), Event (..))
import qualified Devbot.Event      as E

import           Devbot.Service    (Service (..))
import qualified Devbot.Service    as S

import           ColorText


runList :: IO ()
runList = do
        sortOn E._name <$> E.events   >>= mapM_ printEvent
        sortOn S._name <$> S.services >>= mapM_ printService


printService :: Service -> IO ()
-- ^ show name, action, and uptime
printService (Service n c) = do
        uptime <- S.getUptime n
        now <- getTime
        let seconds = (\x -> prettyTime $ now - x) <$> uptime

        putStrLn $ concat
            [ "service: ", printName n
            , pad, decorate (S.action c) blue
            , pad, decorate ("uptime " <> fromMaybe "not running" seconds) cyan, "\n"
            ]
    where
        pad     = "\n    "


printEvent :: Event -> IO ()
printEvent (Event n c d) = do
        now <- getTime
        putStrLn $ concat
            [ printName n, "\n"
            , printAction c, "\n"
            , printInterval c
            , printOptional c d, ", "
            , printNext d now
            , "\n"
            ]


printName :: String -> String
printName name = decorate name green


printAction :: Config -> String
printAction c =
        decorate _action blue
    where
        _action = "    " <> intercalate pad (E.action c)
        pad     = "\n    "


printInterval :: Config -> String
printInterval c =
        decorate ("    every " <> prettyTime (E.interval c)) cyan


printOptional :: Config -> Data -> String
printOptional (Config _ _ req par one) d =
        concat [ maybe "" printErrors $ _errors d
               , printDuration $ E._duration d
               , maybe "" printRequire req
               , printParallel par
               , printOneShell one
               ]
    where
        printErrors :: Integer -> String
        printErrors 1 = ", " <> decorate "1 error" red
        printErrors x = ", " <> decorate (show x <> " errors") red

        printDuration :: Integer -> String
        printDuration 0 = ", " <> decorate "instant" cyan
        printDuration 1 = ", " <> decorate "took 1 second" cyan
        printDuration x = ", " <> decorate ("took " <> show x <> " seconds") cyan

        printRequire :: String -> String
        printRequire r = ", requires " <> r

        printParallel :: Bool -> String
        printParallel True = ", " <> decorate "parallel" magenta
        printParallel _    = ""

        printOneShell :: Bool -> String
        printOneShell False = ", " <> decorate "isolated" magenta
        printOneShell _     = ""


printNext :: Data -> Integer -> String
printNext (Data _ w _) time
        | w - time > 0 = decorate ("next in " <> t) yellow
        | otherwise    = decorate "now" yellow
    where
        t = prettyTime $ w - time


prettyTime :: Integer -> String
prettyTime = pTime False

pTime :: Bool -> Integer -> String
pTime b i
        | i == 0      = ""
        | i < minute  = before <> showTime i "second"
        | i < hour    = before <> showTime (div i minute) "minute" <> pTime True (mod i minute)
        | i < day     = before <> showTime (div i hour)   "hour"   <> pTime True (mod i hour)
        | i < week    = before <> showTime (div i day)    "day"    <> pTime True (mod i day)
        | i < month   = before <> showTime (div i week)   "week"   <> pTime True (mod i week)
        | i < year    = before <> showTime (div i month)  "month"  <> pTime True (mod i month)
        | otherwise   = before <> showTime (div i year)   "year"   <> pTime True (mod i year)
    where
        before = if b then ", " else ""

        showTime :: Integer -> String -> String
        showTime 1 s = "1 " <> s
        showTime x s = show x <> " " <> s <> "s"


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
