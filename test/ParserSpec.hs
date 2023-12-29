module ParserSpec (spec) where

import           Devbot.Internal.Parser
import           Test.Hspec


spec :: Spec
spec = do
        describe "basics" $ do
            it "hour" $
              p "hourly" `shouldBe` Just 3600

            it "day" $
              p "daily" `shouldBe` Just (3600 * 24)

            it "minute" $
              p "minute" `shouldBe` Just 60

            it "second" $
              p "second" `shouldBe` Just 1

        describe "every other" $
            it "minute" $
              p "every other minute" `shouldBe` Just 120

        describe "every x" $
            it "minute" $
              p "every minute" `shouldBe` Just 60

        describe "every x y" $ do
            it "5 seconds" $
              p "every 5 seconds" `shouldBe` Just 5

            it "4 days" $
              p "every 4 days" `shouldBe` Just (3600 * 24 * 4)

            it "five seconds" $
              p "every five seconds" `shouldBe` Just 5

        describe "x times per y" $ do
            it "3, minute" $
              p "3 times per minute" `shouldBe` Just 20

            it "three, minute" $
              p "three times per minute" `shouldBe` Just 20

            it "60, hour" $
              p "60 times per hour" `shouldBe` Just 60

        describe "error times per y" $ do
            it "junk, minute" $
              p "junk times per minute" `shouldBe` Nothing

            -- explicitly chosing to not support this
            it "sixty, minute" $
              p "sixty times per minute" `shouldBe` Nothing

        describe "number parsing" $
            it "works" $
              map readNumber numberWords `shouldBe` map Just [1..10]

    where
        numberWords = words "one two three four five six seven eight nine ten"



p :: String -> Maybe Integer
p = parseTime
