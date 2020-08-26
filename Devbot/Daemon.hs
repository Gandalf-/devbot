module Devbot.Daemon where

import           Control.Monad           (void)
import           Data.Maybe              (catMaybes)
import           System.Info             (os)
import           System.Process          (Pid, spawnCommand)

import           Devbot.Internal.Persist
import           Devbot.Internal.System

runDaemon :: IO ()
-- ^ start devbot in the background in a platform specific way
runDaemon =
        void $ spawnCommand $ case os of
            "mingw32" -> windows
            _         -> unix
    where
        unix    = "devbot start"
        windows =
            "powershell.exe -Command Start-Process " ++
            "powershell.exe -WindowStyle hidden {devbot start}"

stopDaemon :: IO ()
-- ^ stop devbot and all services, then clear service state so devbot list shows that
-- they're not running
stopDaemon = do
        cx <- defaultContext

        -- stop devbot
        get cx ["devbot", "pid"] >>= \case
            (Just pid) -> terminatePid pid
            Nothing    -> pure ()

        -- stop services
        (catMaybes <$>
            (keys cx ["devbot", "pids"] >>= mapM (getPid cx))
            ) >>= mapM_ terminatePid

        -- clear service state
        del cx ["devbot", "pids"]
        del cx ["devbot", "uptime"]
    where
        getPid :: Context -> String -> IO (Maybe Pid)
        getPid cx n = get cx ["devbot", "pids", n]
