module Devbot.Internal.Directory (
    getDatabasePath,
    getConfigPath,
    ensureDefaultDirectory
)where

import           System.Directory      (createDirectoryIfMissing,
                                        getHomeDirectory)
import           System.FilePath.Posix ((</>))


getDatabasePath :: IO FilePath
getDatabasePath = (</> "db.json") <$> directoryBase

getConfigPath :: IO FilePath
getConfigPath = (</> "config.yml") <$> directoryBase

ensureDefaultDirectory :: IO ()
ensureDefaultDirectory =
        directoryBase >>= createDirectoryIfMissing False


-- | Internal
directoryBase :: IO FilePath
directoryBase = (</> ".devbot") <$> getHomeDirectory
