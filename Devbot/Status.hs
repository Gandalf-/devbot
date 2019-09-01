{-# LANGUAGE LambdaCase #-}

module Devbot.Status
    ( runStatus, getStatus, Status(..)
    ) where

import           System.Info            (os)

import           Devbot.Internal.System (checkDevbotRunning)


data Status
    = Stopped
    | Running
    deriving (Show)


runStatus :: IO ()
runStatus = getStatus >>= printStatus


getStatus :: IO Status
getStatus =
        checkDevbotRunning >>= \case
            True  -> pure Running
            False -> pure Stopped


printStatus :: Status -> IO ()
printStatus s
    | os == "mingw32" = plainStatus s
    | otherwise       = fancyStatus s


fancyStatus :: Status -> IO ()
fancyStatus Running = putStrLn "✓"
fancyStatus Stopped = putStrLn "✗"


plainStatus :: Status -> IO ()
plainStatus Running = putStrLn "+"
plainStatus Stopped = putStrLn "x"
