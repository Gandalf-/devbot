module Devbot.Internal.Monitor (
    since,
    pathChangedSince,
    Seconds
) where

import           Control.Exception
import           Data.Time.Clock
import           System.Directory
import           Text.Regex.TDFA

import           Devbot.Internal.Common

type Seconds = Integer

since :: Seconds -> UTCTime -> UTCTime
-- ^ create a new UTCTime that `seconds` before `now`
since seconds now =
        now { utctDayTime = utctDayTime now - shift}
    where
        shift = secondsToDiffTime seconds


pathChangedSince :: Maybe String -> UTCTime -> FilePath -> IO Bool
-- ^ check the path for any file or directory with a modification time newer than
-- the time provided. directories' children are checked recursively
pathChangedSince regex time ('~':'/':ps) = do
        home <- getHomeDirectory
        pathChangedSince regex time (home <> "/" <> ps)

pathChangedSince regex time path =
        falseOnIOException path $ doesFileExist path >>= \case
            True  -> fileChangedSince regex time path
            False -> treeChangedSince regex time path


-- | Internal

fileChangedSince :: Maybe String -> UTCTime -> FilePath -> IO Bool
-- ^ compare modification time, return False if the file matches the ignore
-- regex, this happens to work for a single directory as well
fileChangedSince (Just regex) time path
        | path =~ regex = pure False
        | otherwise     = fileChangedSince Nothing time path
fileChangedSince Nothing time path = do
        file <- getModificationTime path
        pure (diffUTCTime file time > 0)


treeChangedSince :: Maybe String -> UTCTime -> FilePath -> IO Bool
-- ^ compare modification time for the directory itself, then the entries
treeChangedSince regex t path =
        fileChangedSince regex t path >>= \case
            True  -> pure True
            False -> do
                children <- map buildPath <$> listDirectory path
                or <$> mapM (pathChangedSince regex t) children
    where
        buildPath child = path <> "/" <> child


falseOnIOException :: String -> IO Bool -> IO Bool
-- ^ catch any IO exception and return false for the computation
falseOnIOException context io = catch io handler
    where
        handler :: IOException -> IO Bool
        handler e = do
            logger $ "monitor: " <> context <> " " <> show e
            pure False
