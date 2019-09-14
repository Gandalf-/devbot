{-# LANGUAGE LambdaCase #-}

module Devbot.Status
    ( runStatus, getStatus, Status(..)
    ) where

import           System.Info            (os)

import           Devbot.Internal.System (checkDevbotRunning)


runStatus :: IO ()
runStatus = getStatus >>= putStrLn . printStatus

data Status = Stopped | Running
    deriving (Show)

getStatus :: IO Status
getStatus =
        (\case
            True  -> Running
            False -> Stopped
        ) <$> checkDevbotRunning


-- | utilities

printStatus :: Status -> String
-- ^ wrapper since some (most?) windows terminals don't support unicode
printStatus s
    | os == "mingw32" = plainStatus s
    | otherwise       = fancyStatus s

fancyStatus :: Status -> String
-- ^ pretty unicode symbols
fancyStatus Running = "✓"
fancyStatus Stopped = "✗"

plainStatus :: Status -> String
-- ^ plain symbols
plainStatus Running = "+"
plainStatus Stopped = "x"
