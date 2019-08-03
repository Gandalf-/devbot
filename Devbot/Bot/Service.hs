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
getTasks = map makeTask <$> services
    where
        makeTask :: Service -> Task
        makeTask s = Task s Nothing 0


handle :: ContextF -> Task -> IO Task
handle _ (Task service (Just h) s) = do
        -- ^ already running, see if it's still alive
        state <- checkHandles [h]
        case state of
            StillRunning -> pure runningTask
                -- the service is still running, nothing to do

            AllSuccess -> do
                -- the service quit happily?
                logger $ "service: " <> _name service <> " quit without error"
                pure resetTask

            AnyFailure -> do
                -- the service quit with an error
                logger $ "service " <> _name service <> " quit with an error"
                pure resetTask
    where
        runningTask  = Task service (Just h) s
        resetTask    = Task service Nothing 0

handle cxf (Task s Nothing _) = do
        -- ^ start the service, save start time
        h <- spawnProcess process args
        flushUptime cxf s
        Task s (Just h) <$> getTime
    where
        process  = head elements
        args     = safeTail elements
        elements = words $ action $ _config s

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

        stale      = filter (\x -> hash x `elem` staleNames) old
        neededNew  = filter (\x -> hash x `notElem` oldNames) new
        neededOld  = filter (\x -> hash x `elem` notStaleNames) old

        staleNames    = filter (`notElem` newNames) oldNames
        notStaleNames = filter (`elem` newNames) oldNames

        oldNames = map hash old
        newNames = map hash new

        hash :: Task -> String
        hash t = _name (_service t) <> (action . _config $ _service t)


flushUptime :: ContextF -> Service -> IO ()
flushUptime cxf service = do
        cx <- cxf
        now <- getTime
        set cx ["devbot", "uptime", _name service] now


kill :: Task -> IO ()
kill task = case _process task of
        Nothing  -> pure ()
        (Just h) -> do
            logger $ "terminating " <> name <> " due to config change"
            terminateProcess h
            void $ waitForProcess h
            logger $ name <> " terminated"
    where
        name = _name (_service task)
