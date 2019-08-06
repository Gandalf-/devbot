{-# LANGUAGE LambdaCase     #-}

module Devbot.Status
    ( runStatus
    ) where

import           System.Directory (doesFileExist)
import           System.Info      (os)

import qualified Devbot.Load      as L
import           Devbot.Persist


runStatus :: IO ()
runStatus =
        checkAlive >>= \case
            True  -> checkStarted
            False -> status Database
    where
        checkAlive :: IO Bool
        checkAlive = do
            cx <- defaultContext
            not . null <$> keys cx ["devbot"]


checkStarted :: IO ()
checkStarted =
        L.defaultPidPath >>= doesFileExist >>= \case
            True  -> checkRunning
            False -> status Stopped


checkRunning :: IO ()
checkRunning =
        L.checkRunning >>= \case
            True  -> status Running
            False -> status StalePid


data Status =
      Stopped
    | Running
    | StalePid
    | Database

status :: Status -> IO ()
status s
    | os == "mingw32" = plainStatus s
    | otherwise       = fancyStatus s


fancyStatus :: Status -> IO ()
fancyStatus Running = putStrLn "✓"
fancyStatus Stopped = putStrLn "✗"
fancyStatus x       = plainStatus x


plainStatus :: Status -> IO ()
plainStatus Running  = putStrLn "+"
plainStatus Stopped  = putStrLn "x"
plainStatus StalePid = putStrLn "?"
plainStatus Database = putStrLn "!"
