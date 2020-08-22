{-# LANGUAGE LambdaCase #-}

{-|
Module      : Devbot.Event.Runtime
Description : Handles creating and monitoring event's tasks, which are shell commands to
              run at a particular interval, potentially with other requirements
Copyright   : (c) Austin 2019
License     : MIT
Maintainer  : austin@anardil.net
Stability   : stable
Portability : POSIX, Windows 10
-}

module Devbot.Event.Runtime where

import           Data.List               (intercalate)
import           Data.Maybe
import           System.Exit             (ExitCode (..))
import           System.Info             (os)
import           System.Process
import           Data.Time.Clock

import           Devbot.Event.Config
import           Devbot.Internal.Common
import           Devbot.Internal.Persist
import           Devbot.Internal.Monitor


data Task = Task
        { _event   :: Event            -- ^ Event configuration data
        , _process :: [ProcessHandle]  -- ^ currently running processes
        , _cmds    :: Maybe [String]   -- ^ future commands to run
        , _start   :: Integer          -- ^ start time for first command
        }

instance Show Task where
    -- ^ we need a custom show instance since ProcessHandle doesn't have one
    show (Task e _ c s) =
        intercalate ", " [show e, "<process hande>", show c, show s]

instance Eq Task where
    -- ^ tasks without processes are compared by field, otherwise they're always inequal
    (==) (Task a [] b c) (Task x [] y z) = a == x && b == y && c == z
    (==) _ _                             = False


getTasks :: IO [Task]
-- ^ create tasks from the event information
getTasks = map makeTask <$> events
    where
        makeTask :: Event -> Task
        makeTask event = Task event [] Nothing 0


noRunners :: [Task] -> Bool
-- ^ do any of these tasks have running process handles?
noRunners []                         = True
noRunners (Task _ [] Nothing _ : xs) = noRunners xs
noRunners (Task{} : _)               = False


handle :: ContextF -> Task -> IO Task
handle cxf task@(Task e [] Nothing _) = do
        -- not currently running, check to see if we should
        now <- getTime

        if now > _when (_data e)
            -- ready, see if our requirements are met
            then check cxf task

            -- not ready, keep waiting
            else pure task

handle cxf task@(Task _ hs Nothing _) =
        -- we're running, but there's nothing to do after this
        checkHandles hs >>= \case
            StillRunning -> pure task
            AllSuccess   -> success cxf task
            AnyFailure   -> failure cxf task

handle cxf task@(Task _ hs (Just []) _) =
        -- we're running, but there's nothing to do after this
        checkHandles hs >>= \case
            StillRunning -> pure task
            AllSuccess   -> success cxf task
            AnyFailure   -> failure cxf task

handle cxf task@(Task event hs cmds startTime) =
        -- we're running, and there are more commands to run once these are done
        checkHandles hs >>= \case
            StillRunning -> pure task

            AllSuccess ->
                -- clear handles, preserve start time, and try to start the next command
                check cxf $ Task event [] cmds startTime

            AnyFailure ->
                -- something failed, we'll give up here, even though there's more to do
                failure cxf task


check :: ContextF -> Task -> IO Task
-- ^ events may have requirements, which are shell snippets we must execute before the
-- event's actions can be run. if the requirement exits non-zero, we treat that as
-- failure. events without configured requirements eseentially skip this
--
-- also checks for monitor commands. if these succeed, the command is counted as
-- a success without having run, if the output is different, then the real
-- action is run as normal
check cxf task@(Task e@(Event n c d) hs cs s) =
        -- if we can't run, wait 30 seconds before trying again
        requirementsMet cxf n c >>= \case
            True -> do
                time <- getTime
                (newEvent, run) <- monitorMet cxf e
                if run
                    then chooseRunner $ task {_event = newEvent }
                    else success cxf $ task {_event = newEvent, _start = time}

            False -> waitTask <$> getTime
    where
        waitTask now = Task (Event n c (backoff now d)) hs cs s

        chooseRunner :: (Task -> IO Task)
        chooseRunner
            | parallel c = runParallel
            | otherwise  = runSerial

        backoff :: Seconds -> Data -> Data
        backoff now _d = _d { _when = now + 30}


runParallel :: Task -> IO Task
-- ^ start all actions immediately, there is no follow on work after this
runParallel task = do
        handles <- mapM spawnCommand actions
        Task event handles Nothing <$> getTime
    where
        actions = action $ _config event
        event   = _event task


runSerial :: Task -> IO Task
-- ^ run each command serially, we haven't started yet
runSerial task@(Task e _ Nothing _)
        | null actions = pure task
            -- this indicates a bad config

        | oneshell $ _config e = do
            h <- spawnCommand cmd
            Task e [h] Nothing <$> getTime

        | otherwise = do
            -- run each action in a separate shell
            h <- spawnCommand firstCmd
            Task e [h] laterCmds <$> getTime
    where
        actions   = action $ _config e
        firstCmd  = head actions
        laterCmds = Just $ tail actions
        cmd       = intercalate " && " $ map braceShell actions

        braceShell x
            | os == "mingw32" = x
            | otherwise       = "{ " <> x <> " ; }"

runSerial task@(Task e _ (Just cs) s)
-- we've already started, start the next cmd
        | null cs   = pure task  -- this should be impossible thanks to 'handle'
        | otherwise = do
            h <- spawnCommand nextCmd
            pure $ Task e [h] remaining s
    where
        nextCmd   = head cs
        remaining = Just $ tail cs


success :: ContextF -> Task -> IO Task
success cxf (Task event@(Event _ c _) _ _ startTime) = do
        -- the command succeeded, set errors to Nothing, and determine next
        -- time to run
        now <- getTime

        let elapsed  = now - startTime
            newEvent = clearErrors $ updateTimes event now next elapsed
            newTask  = Task newEvent [] Nothing 0

        flush cxf newEvent
        pure newTask
    where
        next = nextRun startTime c

        clearErrors :: Event -> Event
        clearErrors e = e { _data = (_data e) { _errors = Nothing } }


failure :: ContextF -> Task -> IO Task
failure cxf (Task event@(Event n _ d) _ _ startTime) = do
        -- the command failed, log the error, increment errors and set next
        -- time to retry
        logger $ concat [
            "running '", n , "' failed, backing off ", show backoff, " seconds"
            ]

        now <- getTime
        let next     = now + backoff
            elapsed  = now - startTime
            newEvent = incrementError $ updateTimes event now next elapsed
            newTask  = Task newEvent [] Nothing 0

        flush cxf newEvent
        pure newTask
    where
        backoff = getBackoff d

        getBackoff :: Data -> Seconds
        getBackoff = maybe 30 (\e -> (e + 1) * 60) . _errors

        incrementError :: Event -> Event
        incrementError e = e {
            _data = (_data e) {
                _errors = Just $ maybe 1 (+ 1) $ _errors $ _data e}
            }


updateTimes :: Event -> Seconds -> Seconds -> Seconds -> Event
-- record when we ran, how long we took to run, and the next time to run
updateTimes e now next elapsed = e {
        _data = (_data e) {
            _lastRun = Just now, _duration = elapsed, _when = next}
        }


nextRun :: Seconds -> Config -> Seconds
nextRun time config = time + interval config


flush :: ContextF -> Event -> IO ()
flush cxf (Event n _ d) = do
        cx <- cxf
        set cx ["devbot", "data", n] d


requirementsMet :: ContextF -> String -> Config -> IO Bool
requirementsMet _   _ (Config _ _ Nothing  _ _ _)  = pure True
requirementsMet cxf n (Config _ _ (Just r) _ _ _) = do
        cx <- cxf
        get cx ["devbot", "requirements", r] >>= \case
            Nothing  -> logger doesntExist >> pure False
            (Just a) -> runCheck a
    where
        runCheck :: String -> IO Bool
        runCheck cmd =
            spawnCommand cmd >>= waitForProcess >>= \case
                ExitSuccess -> pure True
                _           -> logger cmdFailed >> pure False

        doesntExist =
          n <> " references requirement that doesn't exist"

        cmdFailed =
          "requirement " <> r <> " for " <> n <> " not met"

monitorMet :: ContextF -> Event -> IO (Event, Bool)
-- no monitor information provided
monitorMet _   e@(Event _ (Config _ _ _ Nothing  _ _) _) = pure (e, True)

-- monitor command provided
monitorMet cxf e@(Event n (Config _ _ _ (Just (Monitor (Just c) _ _)) _ _) d) = do
        (code, new_output, _) <- readCreateProcessWithExitCode (shell c) ""

        case code of
            ExitSuccess ->
                if new_output == old_output
                    then pure (e, False)
                    else do
                        let new = e {_data = d {_monitorOutput = Just new_output}}
                        flush cxf new
                        pure (new, True)

            _ -> logger cmdFailed >> pure (e, False)
    where
        cmdFailed =
          "monitoring command \"" <> c <> "\" for " <> n <> " failed"

        old_output = fromMaybe "" $ _monitorOutput $ _data e

-- monitor with implied time, typically since the last time we ran
monitorMet _ e@(Event _ (Config _ i _ (Just (Monitor Nothing (Just p) r)) _ _) d) = do
        shift <- monitorShift (_lastRun d) i
        time <- since shift <$> getCurrentTime
        (,) e <$> pathChangedSince r time p

-- something else
monitorMet _ e = fail $ "bad configuration for " <> _name e


monitorShift :: Maybe Seconds -> Seconds -> IO Seconds
-- seconds into the past we should look for changes, if we have our last run,
-- use the time since then. otherwise, use our interval
monitorShift Nothing int = pure int
monitorShift (Just l) _ = do
        now <- getTime
        print $ "using shift " <> show (now - l)
        pure $ now - l
