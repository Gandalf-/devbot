{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Devbot.Event.Config where

import           Data.Aeson
import           Data.Foldable           (asum)
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe
import qualified Data.Text               as T
import           GHC.Generics

import           Devbot.Internal.Parser
import           Devbot.Internal.Persist


class Valid a where
    valid :: a -> Maybe String


-- | Event
-- wrapper for the data stored in 'data' and 'events'
data Event = Event
       { _name   :: !String
       , _config :: !Config
       , _data   :: !Data
       }
    deriving (Show, Eq)

instance Ord Event where
    compare a b = compare (_name a) (_name b)


-- | Data
-- run time information for a devbot action, populated by devbot
data Data = Data
        { _duration      :: !Integer
        , _when          :: !Integer
        , _lastRun       :: !(Maybe Integer)
        , _errors        :: !(Maybe Integer)
        , _monitorOutput :: !(Maybe String)
        }
    deriving (Show, Eq, Generic)

instance FromJSON Data where

instance ToJSON Data where
    toEncoding = genericToEncoding defaultOptions


-- | Monitor
-- sub field of Config for watching paths or running monitoring commands
data Monitor = Monitor
        { command :: Maybe String
        , path    :: Maybe FilePath
        , ignore  :: Maybe String
        }
    deriving (Show, Eq, Generic)

instance Valid Monitor where
    valid (Monitor (Just s) Nothing Nothing)
        | null s    = Just "monitor command may not be empty"
        | otherwise = Nothing

    valid (Monitor Nothing (Just p) _)
        | null p    = Just "monitor path may not be empty"
        | otherwise = Nothing

    valid (Monitor (Just _) (Just _) _) =
        Just "monitor command and path may not be used together"

    valid (Monitor _ Nothing (Just _)) =
        Just "monitor ignore regex requires a path"

    valid _ =
        Just "monitor field must contain either a command or path"

instance FromJSON Monitor where
instance ToJSON Monitor where
    toEncoding = genericToEncoding defaultOptions


-- | Config
-- devbot action specification, comes from config file
data Config = Config
        { action   :: ![String]
        , interval :: !Integer
        , require  :: !(Maybe String)
        , monitor  :: !(Maybe Monitor)
        , parallel :: !Bool
        , oneshell :: !Bool
        }
    deriving (Eq, Show, Generic)

instance Valid Config where
    valid (Config [] _ _ _ _ _) = Just "action field may not be empty"
    valid (Config  _ i r m _ _)
            | i < 1        = Just "interval value must be greater than 1 second"
            | others /= [] = Just $ unlines others
            | otherwise    = Nothing
        where
            others = catMaybes $ catMaybes [validRequire <$> r, valid <$> m]
            validRequire s
                | null s    = Just "`require` may not be empty if provided"
                | otherwise = Nothing

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
        monitor  <- o .:? "monitor"

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
        cx <- defaultContext
        cs <- get cx ["devbot", "events"] :: IO (Maybe ConfigMap)
        ds <- get cx ["devbot", "data"  ] :: IO (Maybe DataMap)

        let configs = HM.toList . fromMaybe (HM.fromList []) $ cs
            datas   = fromMaybe (HM.fromList []) ds

            parse :: (String, Config) -> Event
            parse (name, config) =
                Event name config . fromMaybe defaultData $ HM.lookup name datas

        return $ map parse configs
    where
        defaultData = Data 0 0 Nothing Nothing Nothing
