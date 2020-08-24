{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Devbot.Internal.System where

import           Data.Aeson                (defaultOptions, genericToEncoding)
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Yaml
import           GHC.Generics
import           System.Process

#ifdef mingw32_HOST_OS
-- Windows only
import qualified System.Win32.Process      as P

#else
-- Unix only
import           Control.Monad             (void)
import qualified Data.Scientific           as S
import           System.Exit               (ExitCode (..))
import           System.Posix.Process      as P
import           System.Posix.Types        (CPid)

-- end
#endif

import           Devbot.Event.Config       (Config, DataMap, Valid (..))
import           Devbot.Internal.Directory
import           Devbot.Internal.Persist
import           Devbot.Service.Config     (ServiceConfig)


data FileConfig = FileConfig
    -- this comes directly from our config file
        { events       :: HM.HashMap Text Config
        , requirements :: Maybe (HM.HashMap Text Text)
        , services     :: Maybe (HM.HashMap Text ServiceConfig)
        }
    deriving (Show, Eq, Generic)

instance Valid FileConfig where
    valid (FileConfig e _ _)
            | errors /= [] = Just $ unlines errors
            | otherwise    = Nothing
        where
            errors = catMaybes $ zipWith validWithContext names configs
            (names, configs) = unzip $ HM.toList e

            validWithContext :: Valid a => Text -> a -> Maybe String
            validWithContext t m = (\s -> T.unpack t <> ": " <> s) <$> valid m

instance FromJSON FileConfig where
instance ToJSON FileConfig where
        toEncoding = genericToEncoding defaultOptions


#ifndef mingw32_HOST_OS
-- | we need JSON instances for CPid on Linux

instance ToJSON CPid where
    toJSON pid = Number $ S.scientific (toInteger pid) 0
instance FromJSON CPid where
    parseJSON = withScientific "CPid" $ \o -> maybe
            (fail $ "value is not a number!" ++ show o)
            pure
            (S.toBoundedInteger o)
#endif


loadDefaultConfig :: IO (Either String ())
-- ^ read, parse and persist to the database the default config
loadDefaultConfig = getConfigPath >>= decodeFileEither >>= \case
        (Left err)     -> pure $ Left $ show err
        (Right config) -> case valid config of
            Nothing  -> Right <$> setConfig config
            (Just s) -> pure $ Left s


setConfig :: FileConfig -> IO ()
setConfig fileConfig = do
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
    where
        validData  = HM.filterWithKey (\ k _ -> T.pack k `elem` eventNames)
        eventNames = HM.keys $ events fileConfig


saveDevbotPid :: IO ()
-- ^ determine the current process ID and write it out for 'devbot status' to read later
-- on from a different process
saveDevbotPid = do
        c <- defaultContext
        getMyPid >>= set c ["devbot", "pid"]


getMyPid :: IO Pid
#ifdef mingw32_HOST_OS
getMyPid = P.c_GetCurrentProcessId
#else
getMyPid = P.getProcessID
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
        spawnCommand ("kill -0 " <> show pid <> " 2>/dev/null")
            >>= waitForProcess >>= \case
                ExitSuccess -> pure True
                _           -> pure False
#endif

terminatePid :: Pid -> IO ()
-- ^ kill a process by system PID
#ifdef mingw32_HOST_OS
terminatePid = P.terminateProcessById
#else
terminatePid pid =
        checkPid pid >>= \case
            True  -> spawnCommand ("kill " <> show pid) >>= void . waitForProcess
            False -> pure ()
#endif
