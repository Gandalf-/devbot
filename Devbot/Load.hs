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

import           Devbot.Core           (Config, DataMap)
import           Devbot.Persistence


data FileConfig = FileConfig
    { events       :: HM.HashMap T.Text Config
    , requirements :: Maybe (HM.HashMap T.Text T.Text)
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
setConfig (Left es) = pure $ Left $ show es

setConfig (Right es) = do
        cx <- defaultContext
        ds <- get cx ["devbot", "data"] :: IO (Maybe DataMap)

        let eventNames  = HM.keys $ events es
            currentData = fromMaybe (HM.fromList []) ds

            -- remove runtime data for non-existant events
            validData   = HM.filterWithKey
                (\ k _ -> T.pack k `elem` eventNames) currentData

        -- apply file config, events, requirements
        set cx ["devbot"] es

        -- reapply pre-existing runtime data
        set cx ["devbot", "data"] validData

        pure $ Right ()
