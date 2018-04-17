module Ciphers where

import           Data.Char
import           System.IO

-- Converts a char 'a' to 'z' to an integer 0 to 25
char2int :: Char -> Int
char2int c = ord c - ord 'a'

-- Converts an integer 0 to 25 to a char 'a' to 'z'
int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

-- Shifts char c to the right by n letters; operates only on
-- lower-case letters; wraps at alphabet end
shift :: Int -> Char -> Char
shift n c
  | isLower c = int2char ((char2int c + n) `mod` 26)
  | otherwise = c

{-
vigenere "ally" "meetatdawn" == "mppraeoywy"
-}
vigenere :: String -> String -> String
vigenere ks xs = map (\z -> shift (char2int $ snd z) (fst z)) zs
  where
    zs = zip xs $ cycle ks

{-
(unvigenere "ally" $ vigenere "ally" "meetatdawn") == "meetatdawn"
-}
unvigenere :: String -> String -> String
unvigenere ks xs = map (\z -> shift (negate $ char2int $ snd z) (fst z)) zs
  where
    zs = zip xs $ cycle ks

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter a key: "
  key <- getLine
  putStr "Enter a word: "
  word <- getLine
  putStrLn ("Vigenere   = " ++ vigenere key word)
  putStrLn ("Unvigenere = " ++ unvigenere key (vigenere key word))
