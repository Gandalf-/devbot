{-|
Module      : Devbot.Internal.Healthcheck
Description : Optional healthcheck.io-style ping notifications for actions.

An action can opt in to ping notifications by setting a 'health' URL in its
config. We send a GET to <url>/start when the action begins, <url> when it
succeeds, and <url>/fail when it fails. Pings are best-effort: any network
failure is logged and ignored, never propagated as an action failure.

The 'Pinger' indirection exists so production wires up real HTTP and tests
wire up a recorder — see test/HealthcheckSpec.hs.
-}
module Devbot.Internal.Healthcheck
    ( Pinger
    , PingKind (..)
    , ping
    , buildUrl
    , httpPing
    ) where

import           Control.Exception        (SomeException, try)
import           Data.List                (dropWhileEnd)

import           Network.HTTP.Client      (Manager, httpNoBody, parseRequest,
                                           responseTimeoutMicro)
import qualified Network.HTTP.Client      as HC
import           Network.HTTP.Client.TLS  (newTlsManager)

import           Devbot.Internal.Common   (logger)


type Pinger = String -> IO ()

data PingKind = Start | Success | Fail deriving (Eq, Show)


buildUrl :: PingKind -> String -> String
buildUrl Start   base = trimSlash base <> "/start"
buildUrl Success base = trimSlash base
buildUrl Fail    base = trimSlash base <> "/fail"


trimSlash :: String -> String
trimSlash = dropWhileEnd (== '/')


ping :: Pinger -> PingKind -> Maybe String -> IO ()
ping _ _ Nothing     = pure ()
ping p k (Just base) = p (buildUrl k base)


httpPing :: Pinger
-- ^ production pinger: GET with a 5s response timeout. Any exception is
-- caught and logged. A fresh Manager is created per call — pings are
-- infrequent (rate-limited to 5/min on the server side), so the cost of
-- a TLS handshake here is acceptable in exchange for avoiding shared
-- mutable state.
httpPing url = do
        result <- try go :: IO (Either SomeException ())
        case result of
            Left e  -> logger $ "healthcheck ping to " <> url <> " failed: " <> show e
            Right _ -> pure ()
    where
        go :: IO ()
        go = do
            mgr  <- newTlsManager
            base <- parseRequest url
            let req = base { HC.responseTimeout = responseTimeoutMicro fiveSeconds }
            _ <- httpNoBody req (mgr :: Manager)
            pure ()

        fiveSeconds = 5 * 1000 * 1000
