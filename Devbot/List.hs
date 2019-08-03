module Devbot.List
    ( runList
    ) where

import           Data.List             (intercalate, sortOn)
import           Data.Maybe            (fromMaybe)
import           Data.Time.Clock.POSIX (getPOSIXTime)

import           Devbot.Event          (Config (..), Data (..), Event (..))
import qualified Devbot.Event          as E

import           Devbot.Service        (Service (..))
import qualified Devbot.Service        as S

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
        printOneShell True = ", " <> decorate "one-shell" magenta
        printOneShell _    = ""


printNext :: Data -> Integer -> String
printNext (Data _ w _) time
        | w - time > 0 = decorate ("next in " <> t) yellow
        | otherwise    = decorate "now" yellow
    where
        t = prettyTime $ w - time


prettyTime :: Integer -> String
prettyTime i
        | i <= minute = show i <> " seconds"
        | i <= hour   = show (div i minute) <> " minutes"
        | i <= day    = show (div i hour) <> " hours"
        | otherwise   = show (div i day) <> " days"
    where
        day = 86400
        hour = 3600
        minute = 60


getTime :: IO Integer
getTime = round <$> getPOSIXTime


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
