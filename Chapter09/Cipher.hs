module Chapter09.Cipher where

import           Data.Char

-- This version makes the input uppercase and filters out non-A to Z
{-
caesar 3 "hello, world!" == "KHOORZRUOG"
-}
caesar :: Int -> String -> String
caesar shift input =
  map
    (\letter -> chr $ mod (ord letter + shift - letterA) 26 + letterA)
    cleanedInput
  where
    letterA = ord 'A'
    cleanedInput = lettersOnly . map toUpper $ input
    lettersOnly = filter $ \letter -> letter `elem` ['A' .. 'Z']

{-
(uncaesar 3 $ caesar 3 "hello, world!") == "HELLOWORLD"
-}
uncaesar :: Int -> String -> String
uncaesar shift = caesar (-shift)
