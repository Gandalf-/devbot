{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Devbot.Service where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromMaybe)
import           GHC.Generics

import           Devbot.Persist

data Service = Service
        { _name   :: String
        , _config :: ServiceConfig
        }
    deriving (Show, Eq)


data ServiceConfig = ServiceConfig
    -- ^ this comes directly from our config file
        { action :: String
        , log    :: String
        }
    deriving (Show, Eq, Generic)

instance FromJSON ServiceConfig where

instance ToJSON ServiceConfig where
    toEncoding = genericToEncoding defaultOptions


type ServiceConfigMap = HM.HashMap String ServiceConfig
-- ^ config data

type UptimeMap = HM.HashMap String Integer
-- ^ runtime data


services :: IO [Service]
services = do
        c <- defaultContext
        ss <- get c ["devbot", "services"] :: IO (Maybe ServiceConfigMap)

        let configs = HM.toList . fromMaybe (HM.fromList []) $ ss
            parse :: (String, ServiceConfig) -> Service
            parse (name, config) = Service name config

        pure $ map parse configs


getUptime :: String -> IO (Maybe Integer)
getUptime name = do
        c <- defaultContext
        get c ["devbot", "uptime", name]