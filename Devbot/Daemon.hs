{-# LANGUAGE LambdaCase #-}

module Devbot.Daemon where

import           Control.Monad  (void)
import           Data.Maybe     (catMaybes)

import           Devbot.Load
import           Devbot.Persist
import           System.Info    (os)
import           System.Process (Pid, spawnCommand)

runDaemon :: IO ()
runDaemon =
        void $ spawnCommand $ case os of
            "mingw32" -> windows
            _         -> unix
    where
        unix    = "devbot start & disown"
        windows =
            "powershell.exe -Command Start-Process " ++
            "powershell.exe -WindowStyle hidden {devbot start}"

stopDaemon :: IO ()
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
