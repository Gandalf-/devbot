module TableSpec (spec) where

import           Devbot.Internal.Table
import           Devbot.Internal.Display

import           Test.Hspec


spec :: Spec
spec = do
    -- Internal/Display
    describe "time" $ do
        it "pretty" $ do
            prettyTime 0 `shouldBe` ""

            prettyTime 1 `shouldBe` "1 second"
            prettyTime 4 `shouldBe` "4 seconds"

            prettyTime 61 `shouldBe` "1 minute"
            prettyTime 121 `shouldBe` "2 minutes"

        it "granular" $ do
            granularTime 0 `shouldBe` ""

            granularTime 1 `shouldBe` "1 second"
            granularTime 4 `shouldBe` "4 seconds"

            granularTime 61 `shouldBe` "1 minute, 1 second"
            granularTime 121 `shouldBe` "2 minutes, 1 second"

            granularTime (week + 1) `shouldBe` "1 week, 1 second"
            granularTime (week + 121) `shouldBe` "1 week, 2 minutes"

    -- Internal/Table
    describe "rotate" $
        it "works" $
            rotate nums `shouldBe` [[2, 2], [1, 1]]

    describe "buffer" $ do
        it "center" $ do
            buffer Center 3 "a" `shouldBe` " a "
            buffer Center 4 "a" `shouldBe` " a  "
            buffer Center 1 "apple" `shouldBe` "a"
            buffer Center 5 "apple" `shouldBe` "apple"

        it "left" $ do
            buffer Lefty 3 "a" `shouldBe` "a  "
            buffer Lefty 4 "a" `shouldBe` "a   "
            buffer Lefty 1 "apple" `shouldBe` "a"
            buffer Lefty 5 "apple" `shouldBe` "apple"

    describe "smart buffer" $
        it "center" $ do
            smartBuffer Center ["a", "foo", "bc"]
                `shouldBe` ["  a  ", " foo ", " bc  "]

            smartBuffer Center ["a", "fooz", "bc"]
                `shouldBe` ["  a   ", " fooz ", "  bc  "]
    where
        nums = [[1,2], [1,2]] :: [[Integer]]
        week = 60 * 60 * 24 * 7
