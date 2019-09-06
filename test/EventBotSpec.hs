module EventBotSpec (spec) where

import           Apocrypha.Client

import           Devbot.Event.Config
import           Devbot.Event.Runtime
import           Devbot.Internal.Common

import           Test.Hspec


spec :: Spec
spec = do

        -- | helpers
        describe "updateTime" $
            it "works" $ do
                let updatedEvent = Event "sample" sampleConfig newData
                    newData = Data 20 15 Nothing
                updateTime sampleEvent 15 20 `shouldBe` updatedEvent

        describe "nextRun" $
            it "works" $
                nextRun 0 sampleConfig `shouldBe` 10


        -- | minor IO
        describe "flush" $
            it "works" $ do
                c <- getMemoryContext
                let cxf = pure c

                flush cxf sampleEvent
                d <- get c ["devbot", "data", "sample"] :: IO (Maybe Data)
                d `shouldBe` Just sampleData

        -- | success
        describe "success" $
            it "works" $ do
                -- update elapsed time, clear errors
                c <- getMemoryContext
                now <- getTime

                let cxf = pure c
                    expectedTask = Task e [] Nothing 0
                    e = Event "sample" sampleConfig $ Data now 10 Nothing

                new <- success cxf sampleErrorTask
                new `shouldBe` expectedTask


        -- | failure


        -- | requirements
        describe "requirementsMet" $
            it "no requirements" $ do
                met <- requirementsMet getMemoryContext "sample" sampleConfig
                met `shouldBe` True

        describe "requirementsMet" $
            it "a met requirement" $ do
                -- does real IO, expects 'echo' to exist on the system
                c <- getMemoryContext
                let cxf = pure c
                set c ["devbot", "requirements", "myreq"] "echo a"

                met <- requirementsMet cxf "sample" sampleReqConfig
                met `shouldBe` True

        describe "requirementsMet" $
            it "an unmet requirement" $ do
                -- does real IO, expects 'zdkjfdo' to not exist on the system
                c <- getMemoryContext
                let cxf = pure c
                set c ["devbot", "requirements", "myreq"] "zdkjfdo"

                met <- requirementsMet cxf "sample" sampleReqConfig
                met `shouldBe` False

        describe "requirementsMet" $
            it "an non existant requirement" $ do
                met <- requirementsMet getMemoryContext "sample" sampleReqConfig
                met `shouldBe` False

    where
        sampleTask = Task sampleEvent [] Nothing 0
        sampleErrorTask = Task sampleErrorEvent [] Nothing 0

        sampleEvent = Event "sample" sampleConfig sampleData
        sampleErrorEvent = Event "sample" sampleConfig sampleErrorData

        sampleConfig = Config ["echo a"] 10 Nothing False False
        sampleReqConfig = Config ["echo a"] 10 (Just "myreq") False False

        sampleData = Data 0 0 Nothing
        sampleErrorData = Data 0 1 Nothing
