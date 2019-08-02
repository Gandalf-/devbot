module Devbot.Bot where

import           Control.Concurrent    (threadDelay)
import           Control.Monad         (forever)
import           Data.List
import           Data.Maybe            (catMaybes, isNothing)
import           Data.Time.Clock       (getCurrentTime)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.Exit           (ExitCode (..))
import           System.IO             (BufferMode (..), hSetBuffering, stdout)
import           System.Process        (ProcessHandle, getProcessExitCode,
                                        spawnCommand, waitForProcess)

import           Devbot.Core
import           Devbot.Load           (loadDefaultConfig)
import           Devbot.Persistence


type State = [Task]


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


type ContextF = IO Context

runBot :: IO ()
runBot = do
        putStrLn "devbot starting up"
        hSetBuffering stdout LineBuffering

        forever $ do
            loadDefaultConfig
            events >>= runner cxf 1 . startingState
    where
        cxf = defaultContext


startingState :: [Event] -> State
startingState = map (\ event -> Task event [] Nothing 0)


runner :: ContextF -> Integer -> State -> IO State
-- ^ check each Task for something to do
-- run for n iterations before dropping out so main can refetch the
-- events and start us again
runner cxf runs state =
        if runs > minRunsToRestart && noRunners state
          then pure []
          else do
              threadDelay sleepTime
              mapM (handle cxf) state >>= runner cxf (runs + 1)

    where
        minRunsToRestart = 60 * 2      -- 60 seconds
        sleepTime = oneSecond `div` 2  -- half a second
        oneSecond = 1000000

        noRunners :: State -> Bool
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

handle cxf task@(Task _ hs Nothing _) = do
        -- we're running, but there's nothing to do after this
        state <- checkHandles hs
        case state of
            StillRunning -> pure task
            AllSuccess   -> success cxf task
            AnyFailure   -> failure cxf task

handle cxf task@(Task _ hs (Just []) _) = do
        -- we're running, but there's nothing to do after this
        state <- checkHandles hs
        case state of
            StillRunning -> pure task
            AllSuccess   -> success cxf task
            AnyFailure   -> failure cxf task

handle cxf task@(Task event hs cmds startTime) = do
        -- we're running, and there are more commands to run once these are done
        state <- checkHandles hs
        case state of
            StillRunning -> pure task

            AllSuccess ->
                -- clear handles, preserve start time, and try to start the next command
                check cxf $ Task event [] cmds startTime

            AnyFailure ->
                -- something failed, we'll give up here, even though there's more to do
                failure cxf task


check :: ContextF -> Task -> IO Task
check cxf task@(Task (Event n c d) hs cs s) = do
        -- if we can't run, wait 30 seconds before trying again
        now <- getTime
        met <- requirementsMet cxf n c
        if met
            then chooseRunner task
            else pure $ Task (Event n c (backoff now d)) hs cs s
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
        | null actions = pure task  -- this indicates a bad config
        | otherwise    = do
            h <- spawnCommand firstCmd
            Task e [h] laterCmds <$> getTime
    where
        actions   = action $ _config e
        firstCmd  = head actions
        laterCmds = Just $ tail actions

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
nextRun time (Config _ _interval _ _) = time + _interval


flush :: ContextF -> Event -> IO ()
flush cxf (Event n _ d) = do
        cx <- cxf
        set cx ["devbot", "data", n] d


logger :: String -> IO ()
logger msg = do
        time <- getCurrentTime
        putStrLn $ "devbot: " <> show time <> " " <> msg


requirementsMet :: ContextF -> String -> Config -> IO Bool
requirementsMet _   _ (Config _ _ Nothing _)  = pure True
requirementsMet cxf n (Config _ _ (Just r) _) = do
        cx  <- cxf
        req <- get cx ["devbot", "requirements", r]

        case req of
            Nothing  -> logger doesntExist >> pure False
            (Just a) -> runCheck a
    where
        runCheck :: String -> IO Bool
        runCheck cmd = do
            code <- spawnCommand cmd >>= waitForProcess
            case code of
                ExitSuccess -> pure True
                _           -> logger cmdFailed >> pure False

        doesntExist =
          n <> " references requirement that doesn't exist"

        cmdFailed =
          "requirement " <> r <> " for " <> n <> " not met"


getTime :: IO Integer
getTime = round <$> getPOSIXTime


data HandlesState =
      StillRunning
    | AllSuccess
    | AnyFailure

checkHandles :: [ProcessHandle] -> IO HandlesState
checkHandles hs = do
        codes <- mapM getProcessExitCode hs

        pure $ if any isNothing codes
            then StillRunning

            -- all processes are finished, how did they finish?
            else if all (== ExitSuccess) $ catMaybes codes
                then AllSuccess
                else AnyFailure
