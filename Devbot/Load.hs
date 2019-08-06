{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}

module Devbot.Load where

import           Data.Aeson            (defaultOptions, genericToEncoding)
import qualified Data.HashMap.Strict   as HM
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import           Data.Yaml
import           GHC.Generics
import           System.Directory      (getHomeDirectory)
import           System.FilePath.Posix ((</>))

#ifdef mingw32_HOST_OS
import qualified System.Win32.Process  as P
#else
import           System.Exit           (ExitCode (..))
import           System.Posix.Process  as P
import           System.Process        (spawnCommand, waitForProcess)
#endif

import           Devbot.Event          (Config, DataMap)
import           Devbot.Persist
import           Devbot.Service        (ServiceConfig, UptimeMap)


data FileConfig = FileConfig
    -- ^ this comes directly from our config file
        { events       :: HM.HashMap T.Text Config
        , requirements :: Maybe (HM.HashMap T.Text T.Text)
        , services     :: Maybe (HM.HashMap T.Text ServiceConfig)
        }
    deriving (Show, Eq, Generic)

instance FromJSON FileConfig where

instance ToJSON FileConfig where
        toEncoding = genericToEncoding defaultOptions


loadDefaultConfig :: IO (Either String ())
-- ^ read, parse and persist to the database the default config
loadDefaultConfig = defaultConfigPath >>= runLoadConfig


runLoadConfig :: FilePath -> IO (Either String ())
-- ^ read, parse and persist to the database a specific config
runLoadConfig path = decodeFileEither path >>= setConfig


defaultConfigPath :: IO FilePath
-- ^ produce the default config location. this should use XdgConfig ideally
defaultConfigPath =
        (</> ".devbot" </> "config.yml") <$> getHomeDirectory


defaultPidPath :: IO FilePath
-- ^ produce the default pid location. this should use XdgConfig ideally
defaultPidPath =
        (</> ".devbot" </> "pid") <$> getHomeDirectory


setConfig :: Either ParseException FileConfig -> IO (Either String ())
setConfig (Left err) = pure $ Left $ show err

setConfig (Right fileConfig) = do
        cx <- defaultContext
        ds <- get cx ["devbot", "data"]   :: IO (Maybe DataMap)
        du <- get cx ["devbot", "uptime"] :: IO (Maybe UptimeMap)

        let eventNames  = HM.keys $ events fileConfig
            currentData = fromMaybe (HM.fromList []) ds

            -- remove runtime data for non-existant events
            validData   = HM.filterWithKey
                (\ k _ -> T.pack k `elem` eventNames) currentData

        -- apply file config: events, requirements, services
        set cx ["devbot"] fileConfig

        -- reapply pre-existing event runtime data
        set cx ["devbot", "data"] validData

        -- reapply pre-existing service runtime data
        set cx ["devbot", "uptime"] du

        pure $ Right ()


savePid :: IO ()
-- ^ determine the current process ID and write it out for 'devbot status'
savePid = defaultPidPath >>= writePid
    where
        writePid path = getPid >>= writeFile path . show
#ifdef mingw32_HOST_OS
        getPid = P.c_GetCurrentProcessId
#else
        getPid = P.getProcessID
#endif


checkRunning :: IO Bool
checkRunning = do
        pid <- defaultPidPath >>= readFile

#ifdef mingw32_HOST_OS
        P.withTh32Snap P.tH32CS_SNAPPROCESS Nothing (\ snapHandle ->
            not . null . filter (\ (candidate, _, _, _, _) -> show candidate == pid)
                <$> P.th32SnapEnumProcesses snapHandle
            )

#else
        code <- spawnCommand ("kill -0 " <> pid) >>= waitForProcess
        pure $ case code of
            ExitSuccess -> True
            _           -> False
#endif
