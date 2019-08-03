module Devbot.Bot.Core where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (void)
import           System.IO          (BufferMode (..), hSetBuffering, stdout)

import           Devbot.Bot.Common
import qualified Devbot.Bot.Event   as E
import qualified Devbot.Bot.Service as S
import           Devbot.Load        (loadDefaultConfig)
import           Devbot.Persist


data State = State [E.Task] [S.Task]


runBot :: IO ()
-- ^ do one time setup, then start the state machine
runBot = do
        putStrLn "devbot starting up"
        hSetBuffering stdout LineBuffering
        void . stateMachine $ State [] []


stateMachine :: State -> IO State
-- ^ main loop after start up. runner iterations fall back to here for
-- a periodic config refresh. service tasks must be checked against the
-- new config, but otherwise left alone
stateMachine (State _ oldServiceTasks) = do

        loaded <- loadDefaultConfig
        case loaded of
            Left err -> do
                -- ^ we failed to load the config, we have to give up here
                logger $ "config parse error: " <> err
                pure $ State [] []

            Right _  -> do
                -- ^ we have our new config, merge with previous, and start
                es <- E.getTasks
                ss <- S.getTasks >>= S.merge S.kill oldServiceTasks

                runner cxf 1 (State es ss) >>= stateMachine
    where
        cxf = defaultContext


runner :: ContextF -> Integer -> State -> IO State
-- ^ check each Task for something to do
-- run for n iterations before dropping out so main can refetch the
-- events and start us again
runner cxf runs (State ets sts) =
        if runs > minRunsToRestart && E.noRunners ets

          -- we've hit our iteration limit and are currently idle
          then pure refreshState

          -- do the work!
          else do
              threadDelay sleepTime

              handledEvents   <- mapM (E.handle cxf) ets
              handledServices <- mapM (S.handle cxf) sts

              runner cxf (runs + 1) $ State handledEvents handledServices
    where
        minRunsToRestart = 60 * 2      -- 60 seconds
        sleepTime = oneSecond `div` 2  -- half a second
        oneSecond = 1000000

        -- clear event tasks (none are active), but preserve service tasks
        refreshState = State [] sts
