{-|
Module      : Devbot.Internal.Persist
Description : Interface for the rest of the library to a persistent database, the
              dependency on 'apocrypha' is only used here, so another database with
              the same API could be used sometime in the future.
Copyright   : (c) Austin 2019
License     : MIT
Maintainer  : austin@anardil.net
Stability   : stable
Portability : POSIX, Windows 10
-}

module Devbot.Internal.Persist (
    Context, defaultContext, get, set, keys, del
) where

import           Apocrypha.Client          hiding (defaultContext)
import           Devbot.Internal.Directory


defaultContext :: IO Context
-- ^ provide our own default context, which uses the serverless context for simplicity.
-- if there was a database available on the network, we could use it, but choose not to
defaultContext = do
    path <- getDatabasePath
    getContext (Serverless path)
