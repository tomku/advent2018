module Day_02Test where

import           Test.Tasty.HUnit
import           Control.Arrow
import           Data.List
import           Day_02

assertDoubleTriple :: String -> (Bool, Bool) -> String -> Assertion
assertDoubleTriple name expected input =
    assertEqual name expected (containsDouble &&& containsTriple $ input)

ex1 :: String
ex1 = "abcdef"
unit_ex1 :: Assertion
unit_ex1 = assertDoubleTriple "ex1" (False, False) ex1

ex2 :: String
ex2 = "bababc"
unit_ex2 :: Assertion
unit_ex2 = assertDoubleTriple "ex2" (True, True) ex2

ex3 :: String
ex3 = "abbcde"
unit_ex3 :: Assertion
unit_ex3 = assertDoubleTriple "ex3" (True, False) ex3

ex4 :: String
ex4 = "abcccd"
unit_ex4 :: Assertion
unit_ex4 = assertDoubleTriple "ex4" (False, True) ex4

ex5 :: String
ex5 = "aabcdd"
unit_ex5 :: Assertion
unit_ex5 = assertDoubleTriple "ex5" (True, False) ex5

ex6 :: String
ex6 = "abcdee"
unit_ex6 :: Assertion
unit_ex6 = assertDoubleTriple "ex6" (True, False) ex6

ex7 :: String
ex7 = "ababab"
unit_ex7 :: Assertion
unit_ex7 = assertDoubleTriple "ex7" (False, True) ex7

exs :: String
exs = intercalate "\n" [ex1, ex2, ex3, ex4, ex5, ex6, ex7]
unit_checksum :: Assertion
unit_checksum = assertEqual "checksum" 12 (checksum exs)
