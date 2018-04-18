module Exercises where

import           Data.List
import           Data.Maybe (fromMaybe)

-- Determine the kinds
--
-- 1. *
-- 2. *, * -> *
--
--
-- String processing
--
-- Replace "the" with Nothing
{-
notThe "the"         == Nothing
notThe "blahtheblah" == Just "blahtheblah"
notThe "woot"        == Just "woot"
-}
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x

-- Replace Nothing with "a"
notA :: Maybe String -> String
notA = fromMaybe "a"

{-
replaceThe "the cow loves us" == "a cow loves us"
replaceThe "Now is the time for all good men to come to the aid of their party." ==
  "Now is a time for all good men to come to a aid of their party."
-}
replaceThe :: String -> String
replaceThe s = unwords $ map (notA . notThe) (words s)

--
vowels = "aeiou"

-- Count the number of instances of "the" + a vowel
{-
countTheBeforeVowel "the cow"      == 0
countTheBeforeVowel "the evil cow" == 1
-}
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s) 0
  where
    go [] n = 0 + n
    go (w:ws) n
      | head w `elem` vowels = go ws (1 + n)
      | otherwise = go ws n

-- Return the number of vowels in a word
{-
countVowels "the cow"     == 2
countVowels "Mikolajczak" == 4
-}
countVowels :: String -> Int
countVowels s = length $ filter (`elem` vowels) s

--
-- Validate the word
--
newtype Word' =
  Word' String
  deriving (Eq, Show)

{-
mkWord "ala" == Nothing
mkWord "just" == Just (Word' "just")
-}
mkWord :: String -> Maybe Word'
mkWord s
  | v > c = Nothing
  | otherwise = Just $ Word' s
  where
    v = countVowels s
    c = length s - v

--
-- It's only Natural
--
data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

{-
natToInteger Zero               == 0
natToInteger (Succ Zero)        == 1
natToInteger (Succ (Succ Zero)) == 2
-}
natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + natToInteger x

{-
integerToNat 0    == Just Zero
integerToNat 1    == Just (Succ Zero)
integerToNat 2    == Just (Succ (Succ Zero))
integerToNat (-1) == Nothing
-}
integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just $ go i
  where
    go i'
      | i' == 0 = Zero
      | otherwise = Succ $ go (i' - 1)

--
-- Small library for Maybe
--
{-
isJust (Just 1) == True
isJust Nothing  == False
-}
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

{-
isNothing (Just 1) == False
isNothing Nothing  == True
-}
isNothing :: Maybe a -> Bool
isNothing = not . isJust

{-
mayybee 0 (+1) Nothing  == 0
mayybee 0 (+1) (Just 2) == 3
-}
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing  = x
mayybee x f (Just y) = f y

-- cont. p. 739
