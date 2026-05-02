module HealthcheckSpec (spec) where

import           Data.IORef

import           Devbot.Internal.Healthcheck

import           Test.Hspec


spec :: Spec
spec = do
        describe "buildUrl" $ do
            it "appends /start for Start" $
                buildUrl Start "https://x/ping/abc" `shouldBe` "https://x/ping/abc/start"

            it "leaves base URL alone for Success" $
                buildUrl Success "https://x/ping/abc" `shouldBe` "https://x/ping/abc"

            it "appends /fail for Fail" $
                buildUrl Fail "https://x/ping/abc" `shouldBe` "https://x/ping/abc/fail"

            it "trims a trailing slash before appending" $ do
                buildUrl Start   "https://x/ping/abc/" `shouldBe` "https://x/ping/abc/start"
                buildUrl Success "https://x/ping/abc/" `shouldBe` "https://x/ping/abc"
                buildUrl Fail    "https://x/ping/abc/" `shouldBe` "https://x/ping/abc/fail"

        describe "ping" $ do
            it "is a no-op when no health URL is configured" $ do
                (pinger, recorded) <- mkRecorder
                ping pinger Start   Nothing
                ping pinger Success Nothing
                ping pinger Fail    Nothing
                recorded `shouldReturn` []

            it "calls the pinger with the start URL" $ do
                (pinger, recorded) <- mkRecorder
                ping pinger Start (Just "https://x/ping/abc")
                recorded `shouldReturn` ["https://x/ping/abc/start"]

            it "calls the pinger with the base URL on success" $ do
                (pinger, recorded) <- mkRecorder
                ping pinger Success (Just "https://x/ping/abc")
                recorded `shouldReturn` ["https://x/ping/abc"]

            it "calls the pinger with the fail URL on failure" $ do
                (pinger, recorded) <- mkRecorder
                ping pinger Fail (Just "https://x/ping/abc")
                recorded `shouldReturn` ["https://x/ping/abc/fail"]


mkRecorder :: IO (Pinger, IO [String])
-- ^ test helper: a Pinger that records every URL it was called with, and
-- a reader that returns those URLs in call order. No real HTTP.
mkRecorder = do
        ref <- newIORef []
        let pinger url = modifyIORef ref (url :)
            readback   = reverse <$> readIORef ref
        pure (pinger, readback)
