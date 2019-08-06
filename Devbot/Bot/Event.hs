{-#LANGUAGE LambdaCase #-}

module Devbot.Bot.Event where

import           Data.List         (intercalate)
import           System.Exit       (ExitCode (..))
import           System.Info       (os)
import           System.Process    (ProcessHandle, spawnCommand, waitForProcess)

import           Devbot.Bot.Common
import           Devbot.Event
import           Devbot.Persist


data Task = Task
          { _event   :: Event
          , _process :: [ProcessHandle]
          , _cmds    :: Maybe [String]
          , _start   :: Integer
          }

instance Show Task where
        show (Task e _ c s) =
            intercalate ", " [show e, "<process hande>", show c, show s]

instance Eq Task where
        (==) (Task a [] b c) (Task x [] y z) = a == x && b == y && c == z
        (==) _ _                             = False


getTasks :: IO [Task]
getTasks = map makeTask <$> events
    where
        makeTask :: Event -> Task
        makeTask e = Task e [] Nothing 0


noRunners :: [Task] -> Bool
noRunners []                         = True
noRunners (Task _ [] Nothing _ : xs) = noRunners xs
noRunners (Task{} : _)               = False


handle :: ContextF -> Task -> IO Task
handle cxf task@(Task (Event _ _ d) [] Nothing _) = do
        -- not currently running, check to see if we should
        time <- getTime

        if ready time d
            -- ready, see if our requirements are met
            then check cxf task

            -- not ready, keep waiting
            else pure task
    where
        ready :: Integer -> Data -> Bool
        ready now (Data _ _when _) = now > _when

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
check cxf task@(Task (Event n c d) hs cs s) =
        -- ^ if we can't run, wait 30 seconds before trying again
        requirementsMet cxf n c >>= \case
            True  -> chooseRunner task

            False -> do
                now <- getTime
                pure $ Task (Event n c (backoff now d)) hs cs s
    where
        chooseRunner :: (Task -> IO Task)
        chooseRunner
            | parallel c = runParallel
            | otherwise  = runSerial

        backoff :: Integer -> Data -> Data
        backoff now (Data _d _ _e) = Data _d (now + 30) _e


runParallel :: Task -> IO Task
-- ^ start all actions immediately, there is no follow on work after this
runParallel task = do
        hs <- mapM spawnCommand $ action $ _config event
        Task event hs Nothing <$> getTime
    where
        event = _event task


runSerial :: Task -> IO Task
-- ^ run each command serially, we haven't started yet
runSerial task@(Task e _ Nothing _)
        | null actions = pure task
            -- this indicates a bad config

        | useOneShell os = do
            -- run all actions in one shell, not possible on Windows
            h <- spawnCommand command
            Task e [h] Nothing <$> getTime

        | otherwise = do
            -- run each action in a separate shell
            h <- spawnCommand firstCmd
            Task e [h] laterCmds <$> getTime
    where
        actions   = action $ _config e
        firstCmd  = head actions
        laterCmds = Just $ tail actions

        useOneShell "mingw32" = False
        useOneShell _         = oneshell $ _config e

        command      = intercalate " && " $ map braceShell actions
        braceShell x = "{ " <> x <> " ; }"

runSerial task@(Task e _ (Just cs) s)
-- ^ we've already started, start the next cmd
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
        elapsed <- negate . (startTime -) <$> getTime

        let newEvent = clearErrors $ updateTime event next elapsed
            newTask  = Task newEvent [] Nothing 0

        flush cxf newEvent
        pure newTask
    where
        next = nextRun startTime c

        clearErrors :: Event -> Event
        clearErrors (Event n co (Data du wh _)) =
            Event n co (Data du wh Nothing)


failure :: ContextF -> Task -> IO Task
failure cxf (Task event@(Event n _ d) _ _ startTime) = do
        -- the command failed, log the error, increment errors and set next
        -- time to retry
        logger $ concat ["running ", n
                        , " failed, backing off "
                        , show backoff, " seconds"]

        next <- (+ backoff) <$> getTime
        elapsed <- negate . (startTime -) <$> getTime

        let newEvent = incrementError $ updateTime event next elapsed
            newTask  = Task newEvent [] Nothing 0

        flush cxf newEvent
        pure newTask
    where
        backoff = getBackoff d

        getBackoff :: Data -> Integer
        getBackoff (Data _ _ Nothing)  = 30
        getBackoff (Data _ _ (Just e)) = (e + 1) * 60

        incrementError :: Event -> Event
        incrementError (Event _n _c (Data _d _w Nothing)) =
            Event _n _c (Data _d _w (Just 1))

        incrementError (Event _n _c (Data _d _w (Just e))) =
            Event _n _c (Data _d _w (Just $ e + 1))


updateTime :: Event -> Integer -> Integer -> Event
updateTime (Event n c (Data _ _ e)) newTime elapsed =
        Event n c (Data elapsed newTime e)


nextRun :: Integer -> Config -> Integer
nextRun time config = time + interval config


flush :: ContextF -> Event -> IO ()
flush cxf (Event n _ d) = do
        cx <- cxf
        set cx ["devbot", "data", n] d


requirementsMet :: ContextF -> String -> Config -> IO Bool
requirementsMet _   _ (Config _ _ Nothing  _ _)  = pure True
requirementsMet cxf n (Config _ _ (Just r) _ _) = do
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
