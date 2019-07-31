module Devbot.Status
    ( runStatus
    ) where

import           System.Directory      (doesFileExist, getHomeDirectory)
import           System.Exit           (ExitCode (..))
import           System.FilePath.Posix ((</>))
import           System.Info           (os)
import           System.Process        (spawnCommand, waitForProcess)

import           Devbot.Persistence


runStatus :: IO ()
runStatus = do
        databaseAlive <- checkAlive

        if databaseAlive
            then if os == "mingw32"
                -- nothing further to do on Windows
                then status StalePid

                -- check if we have our pid
                else checkStarted
            else status Database
    where
        checkAlive :: IO Bool
        checkAlive = do
            cx <- defaultContext
            not . null <$> keys cx ["devbot"]


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
pfile = (</> ".devbot" </> "pid") <$> getHomeDirectory

data Status = Stopped
            | Running
            | StalePid
            | Database

status :: Status -> IO ()
status Running  = putStrLn "✓"
status Stopped  = putStrLn "✗"
status StalePid = putStrLn "?"
status Database = putStrLn "!"
