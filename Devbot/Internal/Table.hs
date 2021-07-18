module Devbot.Internal.Table where

import           Data.Char                 (isSpace)
import           Data.List                 (transpose)

import           Devbot.Internal.Display
import           Devbot.Internal.ColorText (Decoration)

type Uptime = Maybe Integer
type CurrentTime = Integer


-- | shared utilities

data Alignment = Lefty | Center

buffer :: Alignment -> Int -> String -> String
-- ^ pad the given string to a particular length, using an alignment scheme
-- this will truncate the string if necessary
buffer Lefty  n s = take n $ s <> cycle " "
buffer Center n s = take n body
    where
        body = pad <> s <> cycle " "
        pad = replicate ((n - length s) `div` 2) ' '

smartBuffer :: Alignment -> [String] -> [String]
-- ^ choose an appropriate buffer pad size given a list of elements so that none are
-- truncated and to limit extra output space
smartBuffer a column = map (buffer a pad) column
    where
        pad = (+ 2) $ maximum $ map length column

rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

addBorder :: Decoration -> [(Decoration, String)] -> [(Decoration, String)]
addBorder d rows = zip ([c, d] <> cs) ([s, border] <> xs)
    where
        (c:cs, s:xs) = unzip rows
        border = replicate width '-'
        width = maximum $ map length (s:xs)

applyBuffer :: Alignment -> [(Decoration, String)] -> Maybe [String]
applyBuffer a ds
        | all (all isSpace) columns = Nothing
        | otherwise                 = Just result
    where
        result   = zipWith colorize colors buffered

        colors   = map fst ds
        buffered = smartBuffer a $ map snd ds
        columns  = tail $ tail buffered   -- ignore header and border
