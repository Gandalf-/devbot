{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Devbot.Internal.System where

import           Data.Aeson            (defaultOptions, genericToEncoding)
import qualified Data.HashMap.Strict   as HM
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Yaml
import           GHC.Generics
import           System.Directory      (getHomeDirectory)
import           System.FilePath.Posix ((</>))
import           System.Process

#ifdef mingw32_HOST_OS
import qualified System.Win32.Process  as P

#else
import           System.Exit           (ExitCode (..))
import           System.Posix.Process  as P
#endif

import           Devbot.Event.Config   (Config, DataMap)
import           Devbot.Internal.Persist
import           Devbot.Service.Config (ServiceConfig)


data FileConfig = FileConfig
    -- ^ this comes directly from our config file
        { events       :: HM.HashMap Text Config
        , requirements :: Maybe (HM.HashMap Text Text)
        , services     :: Maybe (HM.HashMap Text ServiceConfig)
        }
    deriving (Show, Eq, Generic)

instance FromJSON FileConfig where

instance ToJSON FileConfig where
        toEncoding = genericToEncoding defaultOptions


loadDefaultConfig :: IO (Either String ())
-- ^ read, parse and persist to the database the default config
loadDefaultConfig =
        defaultConfigPath >>= decodeFileEither >>= setConfig


defaultConfigPath :: IO FilePath
-- ^ produce the default config location. this should use XdgConfig ideally
defaultConfigPath =
        (</> ".devbot" </> "config.yml") <$> getHomeDirectory


setConfig :: Either ParseException FileConfig -> IO (Either String ())
setConfig (Left err) = pure $ Left $ show err

setConfig (Right fileConfig) = do
        cx <- defaultContext

        -- apply items from config file individually
        set cx ["devbot", "events"]       $ events       fileConfig
        set cx ["devbot", "services"]     $ services     fileConfig
        set cx ["devbot", "requirements"] $ requirements fileConfig

        -- remove runtime data for non-existant events
        (get cx ["devbot", "data"] :: IO (Maybe DataMap)) >>= \case
            Nothing   -> pure ()
            (Just ds) -> set cx ["devbot", "data"] $ validData ds

        -- leave everything else (like pid) alone
        pure $ Right ()
    where
        validData  = HM.filterWithKey (\ k _ -> T.pack k `elem` eventNames)
        eventNames = HM.keys $ events fileConfig


saveDevbotPid :: IO ()
-- ^ determine the current process ID and write it out for 'devbot status'
saveDevbotPid = do
        c <- defaultContext
        retrieveMyPid >>= set c ["devbot", "pid"]


retrieveMyPid :: IO Pid
#ifdef mingw32_HOST_OS
retrieveMyPid = P.c_GetCurrentProcessId
#else
retrieveMyPid = P.getProcessID
#endif


checkDevbotRunning :: IO Bool
-- ^ retrive our last pid from the database and see if it's active
checkDevbotRunning = do
        c <- defaultContext
        get c ["devbot", "pid"] >>= \case
            (Just pid) -> checkPid pid
            Nothing    -> pure False


checkPid :: Pid -> IO Bool
#ifdef mingw32_HOST_OS
checkPid pid =
        P.withTh32Snap P.tH32CS_SNAPPROCESS Nothing (\ snapHandle ->
        not . null . filter (\ (candidate, _, _, _, _) -> candidate == pid)
            <$> P.th32SnapEnumProcesses snapHandle
        )
#else
checkPid pid =
        spawnCommand ("kill -0 " <> show pid) >>= waitForProcess >>= \case
            ExitSuccess -> True
            _           -> False
#endif

terminatePid :: Pid -> IO ()
-- ^ kill a process by system PID
#ifdef mingw32_HOST_OS
terminatePid = P.terminateProcessById
#else
terminatePid pid = callCommand ("kill " <> show pid)
#endif
