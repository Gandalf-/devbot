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
                    newData      = Data 20 15 (Just 10) Nothing Nothing
                updateTimes sampleEvent 10 15 20 `shouldBe` updatedEvent

        describe "valid monitor" $
            it "works" $ do
                -- negative
                valid (Monitor Nothing Nothing Nothing)    `shouldNotBe` Nothing
                valid (Monitor Nothing Nothing (Just "s")) `shouldNotBe` Nothing

                -- positive
                valid (Monitor (Just "s") Nothing Nothing)    `shouldBe` Nothing
                valid (Monitor Nothing (Just "s") Nothing)    `shouldBe` Nothing
                valid (Monitor Nothing (Just "s") (Just "r")) `shouldBe` Nothing

        describe "valid config" $
            it "works" $ do
                -- positive
                valid sampleConfig      `shouldBe` Nothing
                valid sampleReqConfig   `shouldBe` Nothing
                valid sampleMultiConfig `shouldBe` Nothing
                valid sampleShellConfig `shouldBe` Nothing

        describe "monitorShift" $
            it "works" $ do
                -- default to interval
                monitorShift Nothing 50 `shouldReturn` 50

                -- compute time since last run
                now <- getTime
                monitorShift (Just $ now - 30) 10 `shouldReturn` 30

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
                    e = Event "sample" sampleConfig $ Data now 10 (Just now) Nothing Nothing

                success cxf sampleErrorTask `shouldReturn` expectedTask

        -- requirements
        describe "requirementsMet" $ do
            it "no requirements" $
                requirementsMet getMemoryContext "sample" sampleConfig `shouldReturn` True

            it "a met requirement" $ do
                -- does real IO, expects 'echo' to exist on the system
                c <- getMemoryContext
                let cxf = pure c
                set c ["devbot", "requirements", "myreq"] "echo a"

                requirementsMet cxf "sample" sampleReqConfig `shouldReturn` True

            it "an unmet requirement" $ do
                -- does real IO, expects 'zdkjfdo' to not exist on the system
                c <- getMemoryContext
                let cxf = pure c
                set c ["devbot", "requirements", "myreq"] "zdkjfdo"

                requirementsMet cxf "sample" sampleReqConfig `shouldReturn` False

            it "an non existant requirement" $
                requirementsMet getMemoryContext "sample" sampleReqConfig `shouldReturn` False

        -- monitors
        describe "monitorMet" $ do
            it "command first run" $ do
                c <- getMemoryContext
                let cxf = pure c
                (newEvent, run) <- monitorMet cxf sampleMonitorEvent
                run `shouldBe` True
                _monitorOutput (_data newEvent) `shouldBe` Just "a\n"

            it "command second run, same output" $ do
                c <- getMemoryContext
                let cxf   = pure c
                    event = sampleMonitorEvent { _data = sampleData { _monitorOutput = Just "a\n" }}
                (newEvent, run) <- monitorMet cxf event
                run `shouldBe` False
                _monitorOutput (_data newEvent) `shouldBe` Just "a\n"

            it "command second run, different output" $ do
                c <- getMemoryContext
                let cxf   = pure c
                    event = sampleMonitorEvent { _data = sampleData { _monitorOutput = Just "b\n" }}
                (newEvent, run) <- monitorMet cxf event
                run `shouldBe` True
                _monitorOutput (_data newEvent) `shouldBe` Just "a\n"

            it "command failure" $ do
                c <- getMemoryContext
                let cxf   = pure c
                    config = sampleMonitorConfig { monitor = Just $ Monitor (Just "asdokjf") Nothing Nothing }
                    event  = sampleMonitorEvent { _config = config }
                (newEvent, run) <- monitorMet cxf event
                run `shouldBe` False
                _monitorOutput (_data newEvent) `shouldBe` Nothing

        -- serial run
        describe "runSerial" $ do
            it "a single action produces a single handle" $
                testTaskRun runSerial 1 (Just 0) sampleTask

            it "combines all actions into one command, one shell, no pending" $
                testTaskRun runSerial 1 Nothing sampleMultiTask

            it "starts the first action, sets the remaining as pending" $
                testTaskRun runSerial 1 (Just 2) sampleShellTask

        -- parallel run
        describe "runParallel" $
            it "creates handles for all actions immediately" $
                testTaskRun runParallel 3 Nothing sampleMultiTask

    where
        -- defaults, one action
        sampleTask   = Task sampleEvent [] Nothing 0
        sampleEvent  = Event "sample" sampleConfig sampleData
        sampleConfig = Config ["echo a"] 10 Nothing Nothing False False

        -- multiple actions, multiple shells
        sampleShellTask   = sampleTask  { _event = sampleShellEvent }
        sampleShellEvent  = sampleEvent { _config = sampleShellConfig }
        sampleShellConfig = Config ["echo a", "echo b", "echo c"] 10 Nothing Nothing False False

        -- multiple actions, one shell
        sampleMultiTask   = Task sampleMultiEvent [] Nothing 0
        sampleMultiEvent  = Event "sample" sampleMultiConfig sampleData
        sampleMultiConfig = Config ["echo a", "echo b", "echo c"] 10 Nothing Nothing False True

        -- monitor command
        sampleMonitorEvent   = sampleEvent  { _config = sampleMonitorConfig }
        sampleMonitorConfig  = sampleConfig { monitor = sampleCommandMonitor }
        sampleCommandMonitor = Just $ Monitor (Just "echo a") Nothing Nothing

        -- other
        sampleErrorTask  = Task sampleErrorEvent [] Nothing 0
        sampleErrorEvent = Event "sample" sampleConfig sampleErrorData

        sampleReqConfig = Config ["echo a"] 10 (Just "myreq") Nothing False False

        sampleData = Data 0 0 Nothing Nothing Nothing
        sampleErrorData = Data 0 1 Nothing Nothing Nothing


testTaskRun :: (Task -> IO Task) -> Int -> Maybe Int -> Task -> IO ()
testTaskRun f running next task = do
        t <- f task

        length (_process t) `shouldBe` running
        length <$> _cmds t `shouldBe` next

        waitForProcess (head $ _process t) `shouldReturn` ExitSuccess
