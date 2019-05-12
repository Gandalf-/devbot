module Devbot.Status
    ( runStatus
    ) where

import           Apocrypha.Client (keys')
import           System.Directory (doesFileExist, getHomeDirectory)
import           System.Exit      (ExitCode (..))
import           System.Process   (spawnCommand, waitForProcess)


runStatus :: IO ()
runStatus = do
        databaseAlive <- checkAlive

        if databaseAlive
            then checkStarted
            else status Database
    where
        checkAlive :: IO Bool
        checkAlive = not . null <$> keys' ["devbot"]


checkStarted :: IO ()
checkStarted = do
        pExists <- pfile >>= doesFileExist

        if pExists
            then checkRunning
            else status Stopped


checkRunning :: IO ()
checkRunning = do
        pid  <- pfile >>= readFile
        code <- spawnCommand ("kill -0 " <> pid) >>= waitForProcess

        case code of
            ExitSuccess -> status Running
            _           -> status StalePid


pfile :: IO String
pfile = (<> "/.devbot/pid") <$> getHomeDirectory

data Status = Stopped
            | Running
            | StalePid
            | Database

status :: Status -> IO ()
status Running  = putStrLn "✓"
status Stopped  = putStrLn "✗"
status StalePid = putStrLn "?"
status Database = putStrLn "!"