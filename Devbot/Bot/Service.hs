{-# LANGUAGE LambdaCase #-}

module Devbot.Bot.Service where

import           Control.Monad     (void)
import           Data.List         (intercalate)
import           System.Process

import           Devbot.Bot.Common
import           Devbot.Persist
import           Devbot.Service


data Task = Task
        { _service :: Service
        , _process :: Maybe ProcessHandle
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

handle _ (Task service (Just h) s) =
        -- ^ the service is already running, see if it's still alive
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
        runningTask = Task service (Just h) s
        resetTask   = Task service Nothing 0

handle cxf (Task service Nothing _) = do
        -- ^ parse arguments, start the service, save start time
        --
        -- services are run in directly (not in a shell),
        -- so we use spawnProcess, not spawnCommand
        process <- spawnProcess name args
        flushUptime cxf service
        Task service (Just process) <$> getTime
    where
        name     = head elements
        args     = safeTail elements
        elements = words $ action $ _config service

        safeTail [] = []
        safeTail xs = tail xs


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


kill :: Task -> IO ()
-- ^ kill the process, don't progress until it's dead
kill task = case _process task of
        Nothing        -> pure ()

        (Just process) -> do
            logger $ "terminating " <> name <> " due to config change"
            terminate process
            logger $ name <> " terminated"
    where
        name = _name (_service task)

        terminate :: ProcessHandle -> IO ()
        terminate x = do
            terminateProcess x
            void $ waitForProcess x
