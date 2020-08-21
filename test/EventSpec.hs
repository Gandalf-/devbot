module EventSpec (spec) where

import           Apocrypha.Client

import           Devbot.Event.Config
import           Devbot.Event.Runtime
import           Devbot.Internal.Common

import           System.Exit
import           System.Process
import           Test.Hspec


spec :: Spec
spec = do

        -- helpers
        describe "updateTime" $
            it "works" $ do
                let updatedEvent = Event "sample" sampleConfig newData
                    newData = Data 20 15 Nothing Nothing
                updateTime sampleEvent 15 20 `shouldBe` updatedEvent

        describe "nextRun" $
            it "works" $
                nextRun 0 sampleConfig `shouldBe` 10


        -- minor IO
        describe "flush" $
            it "works" $ do
                c <- getMemoryContext
                let cxf = pure c

                flush cxf sampleEvent
                d <- get c ["devbot", "data", "sample"] :: IO (Maybe Data)
                d `shouldBe` Just sampleData

        --  success
        describe "success" $
            it "works" $ do
                -- update elapsed time, clear errors
                c <- getMemoryContext
                now <- getTime

                let cxf = pure c
                    expectedTask = Task e [] Nothing 0
                    e = Event "sample" sampleConfig $ Data now 10 Nothing Nothing

                success cxf sampleErrorTask `shouldReturn` expectedTask


        -- failure


        -- requirements
        describe "requirementsMet" $
            it "no requirements" $
                requirementsMet getMemoryContext "sample" sampleConfig `shouldReturn` True

        describe "requirementsMet" $
            it "a met requirement" $ do
                -- does real IO, expects 'echo' to exist on the system
                c <- getMemoryContext
                let cxf = pure c
                set c ["devbot", "requirements", "myreq"] "echo a"

                requirementsMet cxf "sample" sampleReqConfig `shouldReturn` True

        describe "requirementsMet" $
            it "an unmet requirement" $ do
                -- does real IO, expects 'zdkjfdo' to not exist on the system
                c <- getMemoryContext
                let cxf = pure c
                set c ["devbot", "requirements", "myreq"] "zdkjfdo"

                requirementsMet cxf "sample" sampleReqConfig `shouldReturn` False

        describe "requirementsMet" $
            it "an non existant requirement" $
                requirementsMet getMemoryContext "sample" sampleReqConfig `shouldReturn` False


        -- serial run
        describe "runSerial: simple" $
            it "a single action produces a single handle" $
                testTaskRun runSerial 1 (Just 0) sampleTask

        describe "runSerial: multiple commands in one shell" $
            it "combines all actions into one command, one shell, no pending" $
                testTaskRun runSerial 1 Nothing sampleMultiTask

        describe "runSerial: multiple commands in one separate shells" $
            it "starts the first action, sets the remaining as pending" $
                testTaskRun runSerial 1 (Just 2) sampleShellTask

        -- parallel run
        describe "runParallel" $
            it "creates handles for all actions immediately" $
                testTaskRun runParallel 3 Nothing sampleMultiTask

    where
        -- defaults, one action
        sampleTask = Task sampleEvent [] Nothing 0
        sampleEvent = Event "sample" sampleConfig sampleData
        sampleConfig = Config ["echo a"] 10 Nothing Nothing False False

        -- multiple actions, multiple shells
        sampleShellTask = Task sampleShellEvent [] Nothing 0
        sampleShellEvent = Event "sample" sampleShellConfig sampleData
        sampleShellConfig = Config ["echo a", "echo b", "echo c"] 10 Nothing Nothing False False

        -- multiple actions, one shell
        sampleMultiTask = Task sampleMultiEvent [] Nothing 0
        sampleMultiEvent = Event "sample" sampleMultiConfig sampleData
        sampleMultiConfig = Config ["echo a", "echo b", "echo c"] 10 Nothing Nothing False True

        sampleErrorTask = Task sampleErrorEvent [] Nothing 0
        sampleErrorEvent = Event "sample" sampleConfig sampleErrorData

        sampleReqConfig = Config ["echo a"] 10 (Just "myreq") Nothing False False

        sampleData = Data 0 0 Nothing Nothing
        sampleErrorData = Data 0 1 Nothing Nothing


testTaskRun :: (Task -> IO Task) -> Int -> Maybe Int -> Task -> IO ()
testTaskRun f running next task = do
        t <- f task

        length (_process t) `shouldBe` running
        length <$> _cmds t `shouldBe` next

        waitForProcess (head $ _process t) `shouldReturn` ExitSuccess
