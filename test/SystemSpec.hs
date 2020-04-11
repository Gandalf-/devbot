module SystemSpec (spec) where

import           Devbot.Internal.System

import           Data.Maybe
import           System.Exit
import           System.Info
import           System.Process
import           Test.Hspec


spec :: Spec
spec = do

        -- | pids
        describe "pid" $
            it "sanity" $ do
                alive <- checkPid =<< getMyPid
                alive `shouldBe` True

        describe "pid" $
            it "murder" $ do
                handle <- spawnCommand sleepy
                pid <- getPid handle
                pid `shouldNotBe` Nothing

                let p = fromJust pid
                terminatePid p

                waitForProcess handle `shouldNotReturn` ExitSuccess

    where
        sleepy
            | os == "mingw32" = "timeout 30"
            | otherwise       = "sleep 30"
