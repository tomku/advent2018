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

answer1 = sum . parseInput <$> input

freqs :: String -> [Integer]
freqs nums = scanl (+) 0 (cycle $ parseInput nums)

firstRepeat :: [Integer] -> Maybe Integer
firstRepeat xs = snd <$> find seenBefore (zip [0 ..] xs)
    where seenBefore x = snd x `elem` take (fst x) xs

answer2 = firstRepeat . freqs <$> input

-- Ghetto tests
ex1 = "+1\n-1"
ex2 = "+3\n+3\n+4\n-2\n-4"
ex3 = "-6\n+3\n+8\n+5\n-6"
ex4 = "+7\n+7\n-2\n-7\n-4"

exercises = firstRepeat . freqs <$> [ex1, ex2, ex3, ex4]
testCases = and $ zipWith (==) exercises [Just 0, Just 10, Just 5, Just 14]
