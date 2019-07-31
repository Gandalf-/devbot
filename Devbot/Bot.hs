module Devbot.Bot
    ( runBot
    ) where

import           Control.Concurrent    (threadDelay)
import           Control.Monad         (forever)
import           Data.List             (intercalate)
import           Data.Maybe            (catMaybes, isNothing)
import           Data.Time.Clock       (getCurrentTime)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.Exit           (ExitCode (..))
import           System.Info           (os)
import           System.IO             (BufferMode (..), hSetBuffering, stdout)
import           System.Process        (ProcessHandle, getProcessExitCode,
                                        spawnCommand, waitForProcess)

import           Devbot.Core
import           Devbot.Persistence


type State = [Task]

data Task = Task
          { _event   :: Event
          , _process :: [ProcessHandle]
          , _start   :: Integer
          }


runBot :: IO ()
runBot = do
        putStrLn "devbot starting up"
        hSetBuffering stdout LineBuffering

        forever $
            events >>= runner 1 . startingState


startingState :: [Event] -> State
startingState = map (\ event -> Task event [] 0)


runner :: Integer -> State -> IO State
-- ^ check each Task for something to do
-- run for n iterations before dropping out so main can refetch the
-- events and start us again
runner runs state =
        if runs > minRunsToRestart && noRunners state
          then pure []
          else do
              threadDelay $ 1 * second
              mapM handle state >>= runner (runs + 1)

    where
        second  = 1000000
        minRunsToRestart = 60 * 5

        noRunners :: State -> Bool
        noRunners []                  = True
        noRunners (Task _ []  _ : xs) = noRunners xs
        noRunners (Task{} : _ )       = False


handle :: Task -> IO Task
handle task@(Task (Event _ _ d) [] _) = do
        -- not currently running
        time <- getTime

        if ready time d
            then check task
            else pure task
    where
        ready :: Integer -> Data -> Bool
        ready now (Data _ _when _) = now > _when

handle task@(Task _ hs _) = do
        codes <- mapM getProcessExitCode hs

        if any isNothing codes
            -- something is still running
            then pure task

            -- all processes finished
            else if all (== ExitSuccess) $ catMaybes codes
                     then success task
                     else failure task


check :: Task -> IO Task
check task@(Task (Event n c d) p s) = do
        -- if we can't run, wait 30 seconds before trying again
        now <- getTime
        met <- requirementsMet n c
        if met
            then run task
            else pure $ Task (Event n c (backoff now d)) p s
    where
        backoff :: Integer -> Data -> Data
        backoff now (Data _d _ _e) = Data _d (now + 30) _e


run :: Task -> IO Task
run (Task event@(Event _ (Config actions _ _ True) _) _ _) = do
        -- parallel, start the actions independently, add handles to Task
        hs <- mapM spawnCommand actions
        Task event hs <$> getTime

run (Task event@(Event _ (Config actions _ _ False) _) _ _) = do
        -- serial, build command, start, add handle to task
        h <- spawnCommand cmd
        Task event [h] <$> getTime
    where
        cmd = if os == "mingw32"
            -- powershell or cmd
            then intercalate " ; " actions

            -- POSIX shell
            else intercalate " && " $ map braceShell actions

        braceShell x = "{ " <> x <> " ; }"


success :: Task -> IO Task
success (Task event@(Event _ c _) _ startTime) = do
        -- the command succeeded, set errors to Nothing, and determine next
        -- time to run
        elapsed <- negate . (startTime -) <$> getTime

        let newEvent = clearErrors $ updateTime event next elapsed
            newTask  = Task newEvent [] 0

        flush newEvent
        pure newTask
    where
        next = nextRun startTime c

        clearErrors :: Event -> Event
        clearErrors (Event n co (Data du wh _)) =
            Event n co (Data du wh Nothing)


failure :: Task -> IO Task
failure (Task event@(Event n _ d) _ startTime) = do
        -- the command failed, log the error, increment errors and set next
        -- time to retry
        logger $ concat ["running ", n
                        , " failed, backing off "
                        , show backoff, " seconds"]

        next <- (+ backoff) <$> getTime
        elapsed <- negate . (startTime -) <$> getTime

        let newEvent = incrementError $ updateTime event next elapsed
            newTask  = Task newEvent [] 0

        flush newEvent
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


flush :: Event -> IO ()
flush (Event n _ d) = do
        cx <- defaultContext
        set cx ["devbot", "data", n] d


logger :: String -> IO ()
logger msg = do
        time <- getCurrentTime
        putStrLn $ "devbot: " <> show time <> " " <> msg


requirementsMet :: String -> Config -> IO Bool
requirementsMet _ (Config _ _ Nothing _) = pure True
requirementsMet n (Config _ _ (Just r) _) = do
        cx  <- defaultContext
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
