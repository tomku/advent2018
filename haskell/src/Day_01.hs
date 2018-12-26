module Day_01 where

import           Text.ParserCombinators.ReadP
import           Data.List                      ( find )
import           Advent

input :: IO String
input = readData "day01_part01.txt"

parseLines :: ReadP [Integer]
parseLines = delta `sepBy` char '\n'

parseInput :: String -> [Integer]
parseInput text = (fst . last) $ readP_to_S parseLines text

answer1 :: IO Integer
answer1 = sum . parseInput <$> input

freqs :: String -> [Integer]
freqs nums = scanl (+) 0 (cycle $ parseInput nums)

firstRepeat :: [Integer] -> Maybe Integer
firstRepeat xs = snd <$> find seenBefore (zip [0 ..] xs)
    where seenBefore x = snd x `elem` take (fst x) xs

answer2 :: IO (Maybe Integer)
answer2 = firstRepeat . freqs <$> input
