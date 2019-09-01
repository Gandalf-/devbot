{-# LANGUAGE LambdaCase #-}

module Devbot.Daemon where

import           Control.Monad  (void)

import           Devbot.Load
import           Devbot.Persist
import           System.Info    (os)
import           System.Process (spawnCommand)

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
        get cx ["devbot", "pid"] >>= \case
            (Just pid) -> terminatePid $ read pid
            Nothing    -> pure ()
