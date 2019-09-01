{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Devbot.Event.Config where

import           Data.Aeson
import           Data.Foldable       (asum)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           GHC.Generics

import           Devbot.Internal.Parser
import           Devbot.Internal.Persist


-- | Event
-- wrapper for the data stored in 'data' and 'events'
data Event = Event
       { _name   :: !String
       , _config :: !Config
       , _data   :: !Data
       }
    deriving (Show, Eq)


-- | Data
-- run time information for a devbot action, populated by devbot
data Data = Data
        { _duration :: !Integer
        , _when     :: !Integer
        , _errors   :: !(Maybe Integer)
        }
    deriving (Show, Eq, Generic)

instance FromJSON Data where

instance ToJSON Data where
    toEncoding = genericToEncoding defaultOptions


-- | Config
-- devbot action specification, comes from config file
data Config = Config
        { action   :: ![String]
        , interval :: !Integer
        , require  :: !(Maybe String)
        , parallel :: !Bool
        , oneshell :: !Bool
        }
    deriving (Eq, Show, Generic)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        -- action may be a string or list of string
        action  <- asum [
            do a <- o .: "action"
               case a of
                   (Array _)             -> parseJSON a
                   (Data.Aeson.String s) -> return [T.unpack s]
                   _                     -> fail ""
            ]

        -- interval may be a string or number
        interval <- asum [
            do i <- o .: "interval"
               case i of
                   (Number _)            -> parseJSON i
                   (Data.Aeson.String s) ->
                        case parseTime . T.unpack $ s of
                            Nothing -> fail ""
                            Just v  -> pure v
                   _                     -> fail ""
            ]

        require  <- o .:? "require"

        -- parallel may not exist or be a bool
        parallel <- asum [
            do p <- o .:? "parallel"
               case p of
                   (Just (Bool b)) -> pure b
                   _               -> pure False
             ]

        -- one_shell may not exist or be a bool
        oneshell <- asum [
            do p <- o .:? "oneshell"
               case p of
                   (Just (Bool b)) -> pure b
                   _               -> pure True
             ]

        return Config{..}

instance ToJSON Config where
    toEncoding = genericToEncoding defaultOptions


-- | Library

type ConfigMap = HM.HashMap String Config
type DataMap   = HM.HashMap String Data


events :: IO [Event]
-- ^ retrieve all events stored in the database
events = do
        c <- defaultContext
        cs <- get c ["devbot", "events"] :: IO (Maybe ConfigMap)
        ds <- get c ["devbot", "data"  ] :: IO (Maybe DataMap)

        let configs = HM.toList . fromMaybe (HM.fromList []) $ cs
            datas = fromMaybe (HM.fromList []) ds

            parse :: (String, Config) -> Event
            parse (name, config) =
                Event name config . fromMaybe defaultData $ HM.lookup name datas

        return $ map parse configs
    where
        defaultData = Data 0 0 Nothing
