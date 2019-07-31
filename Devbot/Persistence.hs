module Devbot.Persistence (defaultContext, get, set, keys) where

import           System.Directory      (getHomeDirectory)
import           System.FilePath.Posix ((</>))

import           Apocrypha.Client      hiding (defaultContext)


defaultContext :: IO Context
-- ^ provide our own default context
defaultContext = do
    path <- (</> ".devbot" </> "db.json") <$> getHomeDirectory
    getServerlessContext path
