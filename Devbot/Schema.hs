module Devbot.Schema where


runSchema :: IO ()
runSchema = putStrLn schema


schema :: String
schema = unlines
    [ "events:"
    , "  <event name>:"
    , "    action:   <command string>"
    , "    interval: [<seconds>|hourly|daily|weekly|...]"
    , ""
    , "  <event name>:"
    , "    action:"
    , "      - <command string>"
    , "      - <command string>"
    , "    interval: [<seconds>|hourly|daily|weekly|...]"
    , "    parallel: <bool>"
    , ""
    , "  <event name>:"
    , "    action:   <command string>"
    , "    interval: [<seconds>|hourly|daily|weekly|...]"
    , "    require: <requirement name>"
    , "    monitor: <monitoring command>"
    , ""
    , "requirements:"
    , "  <req name>: <command string>"
    , "  <req name>: <command string>"
    , ""
    , "services:"
    , "  <service name>:"
    , "    action:   [process name] <args>"
    , ""
    , "  <service name>:"
    , "    action:   [process name] <args>"
    ]
