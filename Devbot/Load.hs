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
loadDefaultConfig = defaultConfigPath >>= runLoadConfig


defaultConfigPath :: IO FilePath
defaultConfigPath = (</> ".devbot" </> "config.yml") <$> getHomeDirectory


runLoadConfig :: FilePath -> IO (Either String ())
runLoadConfig path = decodeFileEither path >>= setConfig


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
