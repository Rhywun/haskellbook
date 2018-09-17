module Chapter14.Ciphers where

import           Data.Char       (chr, isLower, ord)
import           Test.QuickCheck

-- Using `listof1` for non-empty list
{-
sample' genWord
  -- ["o","p","h","r","mjrokm","i","fynzzkx","ywbugtejri", ...]
-}
genWord :: Gen String
genWord = listOf1 $ elements ['a' .. 'z']

-- Caesar
--
genShift :: Gen Int
genShift = elements [0 .. 25]

genCaesarParams :: Gen (Int, String)
genCaesarParams = do
  a <- genShift
  b <- genWord
  return (a, b)

caesar :: Int -> String -> String
caesar n = map (\ x -> chr $ mod (ord x + n - a) 26 + a)
  where
    a = ord 'a'

uncaesar :: Int -> String -> String
uncaesar n = caesar (-n)

caesarProp :: Property
caesarProp =
  forAll genCaesarParams (\(n, xs) -> uncaesar n (caesar n xs) == xs)

-- Vigenere
genVigenereParams :: Gen (String, String)
genVigenereParams = do
  a <- genWord
  b <- genWord
  return (a, b)

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
vigenere "ally" "meetatdawn" = "mppraeoywy"
-}
vigenere :: String -> String -> String
vigenere ks xs = map (\z -> shift (char2int $ snd z) (fst z)) zs
  where
    zs = zip xs $ cycle ks

unvigenere :: String -> String -> String
unvigenere ks xs = map (\z -> shift (negate $ char2int $ snd z) (fst z)) zs
  where
    zs = zip xs $ cycle ks

vigenereProp :: Property
vigenereProp =
  forAll genVigenereParams (\(ks, xs) -> unvigenere ks (vigenere ks xs) == xs)

--
runQC = do
  quickCheck caesarProp
  quickCheck vigenereProp
