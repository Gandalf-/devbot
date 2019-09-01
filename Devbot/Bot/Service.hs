{-# LANGUAGE LambdaCase #-}

module Devbot.Bot.Service where

import           Control.Monad     (void)
import           Data.List         (intercalate)
import           System.Process

import           Devbot.Bot.Common
import           Devbot.Load       (terminatePid, checkPid)
import           Devbot.Persist
import           Devbot.Service


data Task = Task
        { _service :: Service
        , _process :: Maybe (Either ProcessHandle Pid)
        , _start   :: Integer
        }

instance Show Task where
        show (Task e _ s) =
            intercalate ", " [show e, "<process hande>", show s]

instance Eq Task where
        (==) a b = _service a == _service b


getTasks :: IO [Task]
-- ^ read the defined services from the database, return them as Tasks
getTasks = map makeTask <$> services
    where
        makeTask :: Service -> Task
        makeTask s = Task s Nothing 0


handle :: ContextF -> Task -> IO Task

handle _ (Task service (Just (Right p)) s) =
        -- ^ the service is already running with a pid, see if it's still alive
        checkPid (show p) >>= \case
            True  -> pure runningTask
                -- the service is still running, nothing to do

            False -> do
                -- the service exited, we don't know why
                logger $ "service: " <> _name service <> " quit with unknown status"
                pure resetTask
    where
        runningTask = Task service (Just $ Right p) s
        resetTask   = Task service Nothing 0

handle _ (Task service (Just (Left h)) s) =
        -- ^ the service is already running with a phandle, see if it's still alive
        checkHandles [h] >>= \case

            StillRunning -> pure runningTask
                -- the service is still running, nothing to do

            AllSuccess -> do
                -- the service quit happily?
                logger $ "service: " <> _name service <> " quit without error"
                pure resetTask

            AnyFailure -> do
                -- the service quit with an error
                logger $ "service: " <> _name service <> " quit with an error"
                pure resetTask
    where
        runningTask = Task service (Just $ Left h) s
        resetTask   = Task service Nothing 0

handle cxf (Task service Nothing _) =
        -- ^ the service is not running
        -- attempt to recover from the database, then
        -- parse arguments, start the service, save start time
        recoverService cxf (_name service) >>= \case
            (Just (pid, uptime)) -> do
                -- ^ already running, and we can recover all the state
                logger $ "service: " <> _name service <> " state recovered from database"
                pure $ Task service (Just . Right $ pid) uptime

            Nothing -> do
                -- ^ not running, start it from scratch
                logger $ "service: " <> _name service <> " starting"
                startService
    where
        name     = head elements
        args     = safeTail elements
        elements = words $ action $ _config service

        safeTail [] = []
        safeTail xs = tail xs

        startService :: IO Task
        startService = do
            -- services are run in directly (not in a shell),
            -- so we use spawnProcess, not spawnCommand
            process <- spawnProcess name args
            flushUptime cxf service
            flushPid cxf service process
            Task service (Just . Left $ process) <$> getTime


recoverService :: ContextF -> String -> IO (Maybe (Pid, Integer))
-- ^ attempt to extract the last known pid and uptime for the given service
recoverService cxf name = do
    cx <- cxf

    uptime <- get cx ["devbot", "uptime", name]
    rawPid <- get cx ["devbot", "pids",   name]
    let pid = read <$> rawPid :: Maybe Pid

    pure $ (,) <$> pid <*> uptime


merge :: (Task -> IO ()) -> [Task] -> [Task] -> IO [Task]
-- ^ given two lists of tasks, kill stale tasks
-- stale meaning that they're running, but are no longer configured
--
-- this is so messy because we want the intersection of old and new,
-- plus 'new - old', and we have to use the 'old' elements in the output
-- when possible
merge func old new = do
        mapM_ func stale
        pure carryOver
    where
        carryOver = neededOld <> neededNew

        stale     = filter (\x -> hash x `elem` staleNames) old
        neededNew = filter (\x -> hash x `notElem` oldNames) new
        neededOld = filter (\x -> hash x `elem` notStaleNames) old

        staleNames    = filter (`notElem` newNames) oldNames
        notStaleNames = filter (`elem` newNames) oldNames

        oldNames = map hash old
        newNames = map hash new

        hash :: Task -> String
        hash t = _name (_service t) <> (action . _config $ _service t)


flushUptime :: ContextF -> Service -> IO ()
-- ^ persist the start time so we can show uptime in 'devbot list'
flushUptime cxf service = do
        cx <- cxf
        now <- getTime
        set cx ["devbot", "uptime", _name service] now


flushPid :: ContextF -> Service -> ProcessHandle -> IO ()
flushPid cxf service process =
        getPid process >>= \case
            (Just pid) -> do
                cx <- cxf
                set cx ["devbot", "pids", name] $ show pid

            Nothing   -> logger $ "service " <> name <> " exited before pid flush"
    where
        name = _name service


kill :: Task -> IO ()
-- ^ kill the process, don't progress until it's dead
kill task = case _process task of
        Nothing        -> pure ()

        (Just something) -> do
            logger $ "terminating " <> name <> " due to config change"
            terminate something
            logger $ name <> " terminated"
    where
        name = _name (_service task)

        terminate :: Either ProcessHandle Pid -> IO ()
        terminate (Left phandle) = do
            terminateProcess phandle
            void $ waitForProcess phandle

        terminate (Right pid) = terminatePid pid
