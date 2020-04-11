module CommonSpec (spec) where

import           Devbot.Internal.Common

import           System.Info
import           System.Process
import           Test.Hspec


spec :: Spec
spec = do

        -- | pids
        describe "checkHandles" $
            it "success" $ do
                hs <- mapM spawnCommand ["echo 1", "echo 2", "echo 3"]
                mapM_ waitForProcess hs
                checkHandles hs `shouldReturn` AllSuccess

        describe "checkHandles" $
            it "running and killed" $ do
                hs <- mapM spawnCommand [sleepy, sleepy, sleepy]
                checkHandles hs `shouldReturn` StillRunning

                mapM_ terminateProcess hs
                mapM_ waitForProcess hs

                checkHandles hs `shouldReturn` AnyFailure

    where
        sleepy
            | os == "mingw32" = "timeout 30"
            | otherwise       = "sleep 30"
