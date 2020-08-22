{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Devbot.Internal.Monitor (
    since,
    pathChangedSince,
    Seconds
) where

import           Control.Exception
import           Data.Time.Clock
import           System.Directory
import           Text.Regex.TDFA

type Seconds = Integer

since :: Seconds -> UTCTime -> UTCTime
since seconds now =
        now { utctDayTime = utctDayTime now - shift}
    where
        shift = secondsToDiffTime seconds

pathChangedSince :: Maybe String -> UTCTime -> FilePath -> IO Bool
pathChangedSince r t ('~':'/':ps) = do
        print "expanding user tilde"
        home <- getHomeDirectory
        pathChangedSince r t (home <> "/" <> ps)

pathChangedSince r t p =
        failOnIOException $
        doesFileExist p >>= \case
            True  -> fileChangedSince r t p
            False -> treeChangedSince r t p

-- | Internal

fileChangedSince :: Maybe String -> UTCTime -> FilePath -> IO Bool
fileChangedSince (Just regex) time path
        | path =~ regex = pure False
        | otherwise     = fileChangedSince Nothing time path
fileChangedSince Nothing time path = do
        print $ "considering " <> path
        file <- getModificationTime path
        pure (diffUTCTime file time > 0)

treeChangedSince :: Maybe String -> UTCTime -> FilePath -> IO Bool
treeChangedSince r t p =
        case r of
            Nothing      -> go
            (Just regex) -> if p =~ regex then pure False else go
    where
        go = do
            children <- map (\c -> p <> "/" <> c) <$> listDirectory p
            or <$> mapM (pathChangedSince r t) children

failOnIOException :: IO Bool -> IO Bool
failOnIOException io = catch io handler
    where
        handler :: IOException -> IO Bool
        handler _ = pure False
