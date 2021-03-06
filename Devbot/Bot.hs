{-|
Module      : Devbot.Bot
Description : High level orchestration of events and services; this manages loading the
              configuration file into the devbot database, periodic reloads, and
              delegating work to the event and service runtime components
Copyright   : (c) Austin 2019
License     : MIT
Maintainer  : austin@anardil.net
Stability   : stable
Portability : POSIX, Windows 10
-}

module Devbot.Bot where

import           Control.Concurrent      (threadDelay)
import           Control.Monad           (void)
import           System.IO               (BufferMode (..), hSetBuffering,
                                          stdout)

import qualified Devbot.Event.Runtime    as E
import           Devbot.Internal.Common
import           Devbot.Internal.Persist
import           Devbot.Internal.System  (loadDefaultConfig)
import qualified Devbot.Service.Runtime  as S


data State = State [E.Task] [S.Task]


runBot :: IO ()
-- ^ do one time setup, then start the state machine
runBot = do
        putStrLn "devbot starting up"
        hSetBuffering stdout LineBuffering
        void $ stateMachine startingState
    where
        startingState = State [] []


stateMachine :: State -> IO State
-- ^ main loop after start up. runner iterations fall back to here for
-- a periodic config refresh. service tasks must be checked against the
-- new config, but otherwise left alone
stateMachine (State _ oldServiceTasks) =

        loadDefaultConfig >>= \case
            Left err -> do
                -- we failed to load the config, we have to give up here
                logger $ "config parse error: " <> err
                pure $ State [] []

            Right _  -> do
                -- we have our new config, merge with previous, and start
                es <- E.getTasks
                ss <- S.getTasks >>= S.merge S.kill oldServiceTasks

                runner cxf 1 (State es ss) >>= stateMachine
    where
        cxf = defaultContext


runner :: ContextF -> Integer -> State -> IO State
-- ^ check each Task for something to do
-- run for n iterations before dropping out so main can refetch the
-- events and start us again
runner cxf iterations (State ets sts) = do
        -- look for request to make an early refresh
        cx <- cxf
        (get cx ["devbot", "reload"] :: IO (Maybe Bool)) >>= \case
            Nothing  -> go minItersToRestart
            (Just _) -> do
                del cx ["devbot", "reload"]
                go 0
    where
        go :: Integer -> IO State
        go neededToRestart =
            if iterations > neededToRestart && E.noRunners ets
                -- we've hit our iteration limit and are currently idle
                then pure refreshState

                -- do the work!
                else do
                    threadDelay sleepTime

                    handledServices <- mapM (S.handle cxf) sts
                    handledEvents   <- mapM (E.handle cxf) ets

                    runner cxf (iterations + 1) $
                        State handledEvents handledServices

        minItersToRestart = 60 * 2      -- 60 seconds
        sleepTime = oneSecond `div` 2   -- half a second
        oneSecond = 1000000

        -- clear event tasks (none are active), but preserve service tasks
        refreshState = State [] sts
