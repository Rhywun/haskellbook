module WordNumber where

import           Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n =
  case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> undefined

{-
digits 123 == [1,2,3]
-}
digits :: Int -> [Int]
digits n = go n []
  where
    go n' ds
      | n' < 10 = n' : ds
      | otherwise = go (n' `div` 10) (n' `mod` 10 : ds)

{-
wordNumber 12324546 == "one-two-three-two-four-five-four-six"
-}
wordNumber :: Int -> String
wordNumber n = intercalate "-" (map digitToWord $ digits n)
