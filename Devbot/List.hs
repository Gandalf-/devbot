module Devbot.List
    ( runList
    ) where

import           ColorText
import           Devbot.Core

import           Data.List             (intercalate, sortOn)
import           Data.Time.Clock.POSIX (getPOSIXTime)


runList :: IO ()
runList = sortOn _name <$> events >>= mapM_ printEvent


printEvent :: Event -> IO ()
printEvent (Event n c d) = do
        time <- now
        putStrLn $ concat
            [ printName n, "\n"
            , printAction c, "\n"
            , printInterval c
            , printOptional c d, ", "
            , printNext d time
            , "\n"
            ]


printName :: String -> String
printName name = decorate name green


printAction :: Config -> String
printAction (Config a _ _ )  =
        decorate _action blue
    where
        _action = "    " ++ intercalate pad a
        pad     = "\n    "


printInterval :: Config -> String
printInterval (Config _ i _) =
        decorate ("    every " ++ prettyTime i) cyan


printOptional :: Config -> Data -> String
printOptional (Config _ _ req) (Data d _ errs) =
        concat [ maybe "" printErrors errs
               , printDuration d
               , maybe "" printRequire req
               ]
    where
        printErrors :: Integer -> String
        printErrors e = ", " ++ decorate (show e ++ " errors") red

        printDuration :: Integer -> String
        printDuration _d =
            ", " ++ decorate ("took " ++ show _d ++ " seconds") cyan

        printRequire :: String -> String
        printRequire r = ", requires " ++ r


printNext :: Data -> Integer -> String
printNext (Data _ w _) time
        | w - time > 0 = decorate ("next in " ++ t) yellow
        | otherwise    = decorate "now" yellow
    where
        t = prettyTime $ w - time


prettyTime :: Integer -> String
prettyTime i
        | i <= minute = show i ++ " seconds"
        | i <= hour   = show (div i minute) ++ " minutes"
        | i <= day    = show (div i hour) ++ " hours"
        | otherwise   = show (div i day) ++ " days"
    where
        day = 86400
        hour = 3600
        minute = 60


now :: IO Integer
now = round <$> getPOSIXTime


blue :: Decoration
blue   = (Blue,   NoColor, Null)

green :: Decoration
green  = (Green,  NoColor, Bold)

yellow :: Decoration
yellow = (Yellow, NoColor, Null)

red :: Decoration
red    = (Red,    NoColor, Null)

cyan :: Decoration
cyan   = (Cyan,   NoColor, Null)
