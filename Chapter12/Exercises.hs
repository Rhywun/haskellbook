module Exercises where

import           Data.List
import           Data.Maybe

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
isJust2 (Just 1) == True
isJust2 Nothing  == False
-}
isJust2 :: Maybe a -> Bool
isJust2 (Just _) = True
isJust2 Nothing  = False

{-
isNothing2 (Just 1) == False
isNothing2 Nothing  == True
-}
isNothing2 :: Maybe a -> Bool
isNothing2 = not . isJust

{-
mayybee 0 (+1) Nothing  == 0
mayybee 0 (+1) (Just 2) == 3
-}
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing  = x
mayybee x f (Just y) = f y

{-
fromMaybe2 0 Nothing  == 0
fromMaybe2 0 (Just 1) == 1
-}
fromMaybe2 :: a -> Maybe a -> a
fromMaybe2 x Nothing  = x
fromMaybe2 x (Just y) = y

{-
fromMaybe3 0 Nothing  == 0
fromMaybe3 0 (Just 1) == 1
-}
fromMaybe3 :: a -> Maybe a -> a
fromMaybe3 x = mayybee x id

{-
listToMaybe2 [1, 2, 3] == Just 1
listToMaybe2 []        == Nothing
 -}
listToMaybe2 :: [a] -> Maybe a
listToMaybe2 []     = Nothing
listToMaybe2 (x:xs) = Just x

{-
maybeToList2 (Just 1) == [1]
maybeToList2 Nothing  == []
-}
maybeToList2 :: Maybe a -> [a]
maybeToList2 Nothing  = []
maybeToList2 (Just x) = [x]

{-
catMaybes2 [Just 1, Nothing, Just 2]   == [1, 2]
catMaybes2 [Nothing, Nothing, Nothing] == []
-}
catMaybes2 :: [Maybe a] -> [a]
catMaybes2 xs = [fromJust x | x <- xs, isJust x] -- or [x | Just x <- xs] !!!

{-
flipMaybe [Just 1, Just 2, Just 3] == Just [1, 2, 3]
flipMaybe [Just 1, Nothing, Just 3] == Nothing
-}
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe xs
  | any isNothing xs = Nothing
  | otherwise = Just (catMaybes2 xs)
    -- Cheated

--
-- Small library for Either
--
{-
lefts2 [Left "foo", Right 3, Left "bar", Right 7, Left "baz"] == ["foo","bar","baz"]
-}
lefts2 :: [Either a b] -> [a]
lefts2 xs = [x | Left x <- xs]
    -- TODO: Rewrite with foldr

{-
rights2 [Left "foo", Right 3, Left "bar", Right 7, Left "baz"] == [3, 7]
-}
rights2 :: [Either a b] -> [b]
rights2 xs = [x | Right x <- xs]
    -- TODO: Rewrite with foldr

{-
partitionEithers2 [Left "foo", Right 3, Left "bar", Right 7, Left "baz"] ==
  (["foo","bar","baz"],[3,7])
-}
partitionEithers2 :: [Either a b] -> ([a], [b])
partitionEithers2 xs = (lefts2 xs, rights2 xs)

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe _ (Left _)  = Nothing
eitherMaybe f (Right y) = Just (f y)

{-
either2 length (*2) (Left "foo") == 3
either2 length (*2) (Right 3)    == 6
-}
either2 :: (a -> c) -> (b -> c) -> Either a b -> c
either2 f _ (Left x)  = f x
either2 _ g (Right y) = g y

-- cont. p. 741
