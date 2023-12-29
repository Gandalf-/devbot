{-# LANGUAGE OverloadedStrings #-}

module SystemSpec (spec) where

import qualified Data.HashMap.Strict    as HM
import           Data.Maybe
import           System.Exit
import           System.Info
import           System.Process
import           Test.Hspec

import           Devbot.Event.Config    (Config (..), Valid (..))
import           Devbot.Internal.System


spec :: Spec
spec = do
        describe "pid" $ do
            it "sanity" $ do
                alive <- checkPid =<< getMyPid
                alive `shouldBe` True

            it "murder" $ do
                handle <- spawnCommand sleepy
                pid <- getPid handle
                pid `shouldNotBe` Nothing

                let p = fromJust pid
                terminatePid p

                waitForProcess handle `shouldNotReturn` ExitSuccess

        describe "valid" $
            it "works" $ do
                -- clean validation
                valid sampleFile `shouldBe` Nothing

                -- validation failure has event name for context
                fromMaybe "" (valid badFile) `shouldStartWith` "sample: "

    where
        sleepy
            | os == "mingw32" = "timeout 30"
            | otherwise       = "sleep 30"

        sampleFile   = FileConfig sampleEvents Nothing Nothing
        sampleEvents = HM.fromList [("example", sampleConfig)]
        sampleConfig = Config ["echo a"] 10 Nothing Nothing False False

        badFile   = sampleFile { events = badEvents }
        badEvents = HM.fromList [("sample", badConfig)]
        badConfig = sampleConfig { action = [] }
