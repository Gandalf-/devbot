

module Devbot.Status
    ( runStatus, getStatus, Status(..)
    ) where

import           Data.Maybe             (mapMaybe)
import           System.Info            (os)

import           Devbot.Event.Config    (Data (..), Event (..), events)
import           Devbot.Internal.System (checkDevbotRunning)


runStatus :: IO ()
runStatus = getStatus >>= putStrLn . printStatus

data Status = Stopped | Running | Error
    deriving (Show)

getStatus :: IO Status
getStatus = do
        running <- checkDevbotRunning
        table running <$> anyErrors
    where
        table False _ = Stopped
        table _ True  = Error
        table _ False = Running

-- | utilities

anyErrors :: IO Bool
-- ^ do any events have outstanding errors?
anyErrors = not . null . mapMaybe (_errors . _data) <$> events

printStatus :: Status -> String
-- ^ wrapper since some (most?) windows terminals don't support unicode
printStatus s
    | os == "mingw32" = plainStatus s
    | otherwise       = fancyStatus s

fancyStatus :: Status -> String
-- ^ pretty unicode symbols
fancyStatus Running = "✓"
fancyStatus Stopped = "✗"
fancyStatus Error   = "!"

plainStatus :: Status -> String
-- ^ plain symbols
plainStatus Running = "+"
plainStatus Stopped = "x"
plainStatus Error   = "!"
