{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Devbot.Load where

import           Data.Aeson            (defaultOptions, genericToEncoding)
import qualified Data.HashMap.Strict   as HM
import           Data.Text             (Text)
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
import           Devbot.Service        (ServiceConfig)


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


savePid :: IO ()
-- ^ determine the current process ID and write it out for 'devbot status'
savePid = do
        c <- defaultContext
        getPid >>= set c ["devbot", "pid"]
    where
        getPid :: IO String
#ifdef mingw32_HOST_OS
        getPid = show <$> P.c_GetCurrentProcessId
#else
        getPid = show <$> P.getProcessID
#endif


checkRunning :: IO Bool
-- ^ retrive our last pid from the database and see if it's active
checkRunning = do
        c <- defaultContext
        get c ["devbot", "pid"] >>= \case
            Nothing  -> pure False
            (Just p) -> checkPid p
    where
        checkPid :: String -> IO Bool
#ifdef mingw32_HOST_OS
        checkPid pid =
             P.withTh32Snap P.tH32CS_SNAPPROCESS Nothing (\ snapHandle ->
             not . null . filter (\ (candidate, _, _, _, _) -> show candidate == pid)
                     <$> P.th32SnapEnumProcesses snapHandle
             )
#else
        checkPid pid =
            spawnCommand ("kill -0 " <> pid) >>= waitForProcess >>= \case
                ExitSuccess -> True
                _           -> False
#endif
