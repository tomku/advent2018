module Day_02 where

import           Data.List
import           Advent

containsMultiple :: Ord a => Int -> [a] -> Bool
containsMultiple n = any (\x -> length x == n) . group . sort

containsDouble :: String -> Bool
containsDouble = containsMultiple 2

containsTriple :: String -> Bool
containsTriple = containsMultiple 3

occurrences :: Eq a => [a] -> a -> Int
occurrences xs x = length $ filter (== x) xs

checksum :: String -> Int
checksum xs = doubles * triples
  where
    ls      = lines xs
    doubles = occurrences (map containsDouble ls) True
    triples = occurrences (map containsTriple ls) True

answer1 :: IO Int
answer1 = checksum <$> readData "day02.txt"
