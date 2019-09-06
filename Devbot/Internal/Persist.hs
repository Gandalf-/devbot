module Devbot.Internal.Persist (
    Context, defaultContext, get, set, keys, del
) where

import           Apocrypha.Client          hiding (defaultContext)
import           Devbot.Internal.Directory


defaultContext :: IO Context
-- ^ provide our own default context
defaultContext = do
    path <- getDatabasePath
    getServerlessContext path
