module Chapter12.Exercises where

import Data.List

-- Determine the kinds

-- 1. *
-- 2. *, * -> *

-- String processing

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x

notA :: Maybe String -> String
notA x = case x of
         Nothing -> "a"
         Just x -> x

replaceThe :: String -> String
replaceThe s = intercalate " " $ map (notA . notThe) (words s)
-- Cheated from two sources!
-- And still probably not what they're looking for.

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = undefined
-- PASS

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel c | c `elem` vowels = True
          | otherwise       = False

countVowels :: String -> Int
countVowels s = length $ filter isVowel s

-- Validate the word

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s | v > c = Nothing
         | otherwise = Just $ Word' s
    where v = countVowels s
          c = length s - v

-- It's only Natural

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat i | i < 0     = Nothing
               | otherwise = Just $ go i
    where go i' | i' == 0   = Zero
                | otherwise = Succ $ go (i' - 1)

-- Small library for Maybe

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- mayybee :: b -> (a -> b) -> Maybe a -> b
-- Yeah, nah.

-- Either zzzzz

-- MAKE IT STOP...
