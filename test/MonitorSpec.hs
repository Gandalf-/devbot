module MonitorSpec (spec) where

import           Devbot.Internal.Monitor

import           Control.Monad
import           Data.Time.Clock
import           System.Directory
import           System.FilePath.Posix   ((</>))
import           Test.Hspec


working :: IO FilePath
working = do
        tmp <- getTemporaryDirectory
        return $ tmp </> "monitor-spec.test"

setup :: IO FilePath
setup = do
        path <- working
        doesFileExist path >>=
            flip when (removeFile path)
        writeFile path "hello there\n"
        pure path

spec :: Spec
spec = do
        describe "since" $
            it "works" $ do
                now <- getCurrentTime

                since 10 now < now `shouldBe` True
                since 0 now == now `shouldBe` True

        describe "pathChanged" $
            it "comparison" $ do
                path <- setup
                now  <- getCurrentTime
                let past = since 10 now

                pathChangedSince Nothing   past path `shouldReturn` True
                pathChangedSince unrelated past path `shouldReturn` True

                pathChangedSince Nothing now  path `shouldReturn` False
                pathChangedSince ignore  past path `shouldReturn` False
    where
        ignore    = Just ".*test"
        unrelated = Just "apple"
