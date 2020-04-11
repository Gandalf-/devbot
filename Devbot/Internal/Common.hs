module Devbot.Internal.Common where

import           Data.Maybe              (catMaybes, isNothing)
import           Data.Time.Clock         (getCurrentTime)
import           Data.Time.Clock.POSIX   (getPOSIXTime)
import           System.Exit             (ExitCode (..))
import           System.Process          (ProcessHandle, getProcessExitCode)

import           Devbot.Internal.Persist


type ContextF = IO Context

logger :: String -> IO ()
logger msg = do
        time <- getCurrentTime
        putStrLn $ "devbot: " <> show time <> " " <> msg


getTime :: IO Integer
getTime = round <$> getPOSIXTime


data HandlesState =
        StillRunning
        | AllSuccess
        | AnyFailure
    deriving (Show, Eq)

checkHandles :: [ProcessHandle] -> IO HandlesState
checkHandles hs = do
        codes <- mapM getProcessExitCode hs

        pure $ if any isNothing codes
            then StillRunning

            -- all processes are finished, how did they finish?
            else if all (== ExitSuccess) $ catMaybes codes
                then AllSuccess
                else AnyFailure
