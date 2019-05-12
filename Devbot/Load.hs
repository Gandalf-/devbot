{-# LANGUAGE DeriveGeneric #-}

module Devbot.Load where

import           Apocrypha.Client      (Context, defaultContext, get, set)
import           Data.Aeson            (defaultOptions, genericToEncoding)
import qualified Data.HashMap.Strict   as HM
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import           Data.Yaml
import           Devbot.Core           (Config, Data, DataMap)
import           GHC.Generics
import           System.Directory      (getHomeDirectory)
import           System.FilePath.Posix ((</>))


data FileConfig = FileConfig
    { events       :: HM.HashMap T.Text Config
    , requirements :: Maybe (HM.HashMap T.Text T.Text)
    }
    deriving (Show, Eq, Generic)

instance FromJSON FileConfig where

instance ToJSON FileConfig where
    toEncoding = genericToEncoding defaultOptions


defaultConfigPath :: IO FilePath
defaultConfigPath = (</> ".devbot/config.yml") <$> getHomeDirectory


runLoadConfig :: FilePath -> IO ()
runLoadConfig path = decodeFileEither path >>= setConfig


setConfig :: Either ParseException FileConfig -> IO ()
setConfig (Left es) = print es

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
        mapM_ (uncurry $ setData cx) $ HM.toList validData
    where
        setData :: Context -> String -> Data -> IO ()
        setData cx name = set cx ["devbot", "data", name]
