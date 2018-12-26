module Day_01Test where

import           Test.Tasty.HUnit
import           Day_01

ex1 :: String
ex1 = "+1\n-1"

ex2 :: String
ex2 = "+3\n+3\n+4\n-2\n-4"

ex3 :: String
ex3 = "-6\n+3\n+8\n+5\n-6"

ex4 :: String
ex4 = "+7\n+7\n-2\n-7\n-4"

findFirstRepeat :: String -> Maybe Integer
findFirstRepeat = firstRepeat . freqs

unit_ex1 :: IO ()
unit_ex1 = assertEqual "ex1" (Just 0) (findFirstRepeat ex1) 

unit_ex2 :: IO ()
unit_ex2 = assertEqual "ex2" (Just 10) (findFirstRepeat ex2) 

unit_ex3 :: IO ()
unit_ex3 = assertEqual "ex3" (Just 5) (findFirstRepeat ex3)

unit_ex4 :: IO ()
unit_ex4 = assertEqual "ex4" (Just 14) (findFirstRepeat ex4) 
