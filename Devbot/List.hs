{-# LANGUAGE LambdaCase #-}

module Devbot.List
    ( runList
    ) where

import           Data.List               (intercalate, sort)

import           Devbot.Internal.Common
import           Devbot.Internal.Display

import           Devbot.Event.Config     (Config (..), Data (..), Event (..))
import qualified Devbot.Event.Config     as E

import           Devbot.Service.Config   (Service (..))
import qualified Devbot.Service.Config   as S


runList :: IO ()
-- ^ build list summaries of all the currently loaded events + services and display them
-- to the console
runList = do
        E.events   >>= sortDo display
        S.services >>= sortDo display
    where
        sortDo :: (Displayable a, Ord a) => (a -> IO ()) -> [a] -> IO ()
        sortDo f s = mapM_ f $ sort s


class Displayable a where
    display :: a -> IO ()

instance Displayable Service where
    -- ^ show name, action, and uptime
    display (Service n c) = do
            now <- getTime
            status <- S.getUptime n >>= \case
               (Just time) -> pure $ "uptime " <> prettyTime (now - time)
               Nothing     -> pure "not running"

            putStrLn $ concat
               [ "service: ", printName n
               , pad, colorize blue (S.action c)
               , pad, colorize cyan status, "\n"
               ]
         where
            pad = "\n    "

instance Displayable Event where
    display (Event n c d) = do
            now <- getTime
            putStrLn $ concat
                [ printName n, "\n"
                , printAction c, "\n"
                , "    ", printInterval c
                , printNext d now
                , printOptional c d
                , "\n"
                ]

printName :: String -> String
printName = colorize green

printAction :: Config -> String
printAction c =
        colorize blue _action
    where
        _action = "    " <> intercalate pad (E.action c)
        pad     = "\n    "


printInterval :: Config -> String
printInterval c =
        colorize cyan $ "every " <> time <> ", "
    where
        time = clean $ prettyTime $ E.interval c


printNext :: Data -> Integer -> String
printNext (Data _ w _) time =
        colorize yellow $
            if w - time > 0
                then "next in " <> t
                else "now"
    where
        t = prettyTime $ w - time


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
        printErrors 1 = ", " <> colorize red "1 error"
        printErrors x = ", " <> colorize red (show x <> " errors")

        printDuration :: Integer -> String
        printDuration 0 = ", " <> colorize cyan "instant"
        printDuration 1 = ", " <> colorize cyan "took 1 second"
        printDuration x = ", " <> colorize cyan ("took " <> show x <> " seconds")

        printRequire :: String -> String
        printRequire r = ", requires " <> r

        printParallel :: Bool -> String
        printParallel True = ", " <> colorize magenta "parallel"
        printParallel _    = ""

        printOneShell :: Bool -> String
        printOneShell False = ", " <> colorize magenta "isolated"
        printOneShell _     = ""
