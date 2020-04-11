module Devbot.ParseCheck (
    runParseCheck
) where

import Devbot.Internal.Parser
import Devbot.Internal.Display


runParseCheck :: [String] -> IO ()
runParseCheck args =
        case parseTime input of
            Nothing  -> putStrLn $ "could not parse '" <> input <> "'!"
            (Just t) -> do
                putStrLn $ "parsed as: " <> show t <> " seconds"
                putStrLn $ "listed as: " <> prettyTime t
    where
        input = unwords args
