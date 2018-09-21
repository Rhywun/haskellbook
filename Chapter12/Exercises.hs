module Chapter12.Exercises where

import           Data.List
import           Data.Maybe

-- Determine the kinds

-- 1. *
-- 2. *, * -> *

-- String processing

-- Replace "the" with Nothing
{-
notThe "the"         -- Nothing
notThe "blahtheblah" -- Just "blahtheblah"
notThe "woot"        -- Just "woot"
-}
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x

-- Replace Nothing with "a"
notA :: Maybe String -> String
notA = fromMaybe "a"

{-
replaceThe "the cow loves us" -- "a cow loves us"
replaceThe "Now is the time for all good men to come to the aid of their party."
  -- "Now is a time for all good men to come to a aid of their party."
-}
replaceThe :: String -> String
replaceThe s = unwords $ map (notA . notThe) (words s)

--

vowels = "aeiou"

-- Count the number of instances of "the" + a vowel
{-
countTheBeforeVowel "the cow"      -- 0
countTheBeforeVowel "the evil cow" -- 1
-}
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (words s) 0
 where
  go [] n = 0 + n
  go (w : ws) n | head w `elem` vowels = go ws (1 + n)
                | otherwise            = go ws n

-- Return the number of vowels in a word
{-
countVowels "the cow"     -- 2
countVowels "Mikolajczak" -- 4
-}
countVowels :: String -> Int
countVowels s = length $ filter (`elem` vowels) s

-- Validate the word

newtype Word' =
  Word' String
  deriving (Eq, Show)

{-
mkWord "ala"  -- Nothing
mkWord "just" -- Just (Word' "just")
-}
mkWord :: String -> Maybe Word'
mkWord s | v > c     = Nothing
         | otherwise = Just $ Word' s
 where
  v = countVowels s
  c = length s - v

-- It's only Natural

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

{-
natToInteger Zero               -- 0
natToInteger (Succ Zero)        -- 1
natToInteger (Succ (Succ Zero)) -- 2
-}
natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + natToInteger x

{-
integerToNat 0    -- Just Zero
integerToNat 1    -- Just (Succ Zero)
integerToNat 2    -- Just (Succ (Succ Zero))
integerToNat (-1) -- Nothing
-}
integerToNat :: Integer -> Maybe Nat
integerToNat i | i < 0     = Nothing
               | otherwise = Just $ go i
 where
  go i' | i' == 0   = Zero
        | otherwise = Succ $ go (i' - 1)

-- Small library for Maybe

{-
isJust' (Just 1) -- True
isJust' Nothing  -- False
-}
isJust' :: Maybe a -> Bool
isJust' (Just _) = True
isJust' Nothing  = False

{-
isNothing' (Just 1) -- False
isNothing' Nothing  -- True
-}
isNothing' :: Maybe a -> Bool
isNothing' = not . isJust'

{-
maybe' 0 (+1) Nothing  -- 0
maybe' 0 (+1) (Just 2) -- 3
-}
maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' x _ Nothing  = x
maybe' _ f (Just y) = f y

{-
fromMaybe' 0 Nothing  -- 0
fromMaybe' 0 (Just 1) -- 1
-}
fromMaybe' :: a -> Maybe a -> a
fromMaybe' x Nothing  = x
fromMaybe' x (Just y) = y

{-
fromMaybe'' 0 Nothing  -- 0
fromMaybe'' 0 (Just 1) -- 1
-}
fromMaybe'' :: a -> Maybe a -> a
fromMaybe'' x = maybe' x id

{-
listToMaybe' [1, 2, 3] -- Just 1
listToMaybe' []        -- Nothing
 -}
listToMaybe' :: [a] -> Maybe a
listToMaybe' []       = Nothing
listToMaybe' (x : xs) = Just x

{-
maybeToList' (Just 1) -- [1]
maybeToList' Nothing  -- []
-}
maybeToList' :: Maybe a -> [a]
maybeToList' Nothing  = []
maybeToList' (Just x) = [x]

{-
catMaybes' [Just 1, Nothing, Just 2]   -- [1, 2]
catMaybes' [Nothing, Nothing, Nothing] -- []
-}
catMaybes' :: [Maybe a] -> [a]
catMaybes' xs = [x | Just x <- xs]

{-
flipMaybe [Just 1, Just 2, Just 3]  -- Just [1, 2, 3]
flipMaybe [Just 1, Nothing, Just 3] -- Nothing
-}
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe xs | any isNothing xs = Nothing
             | otherwise        = Just (catMaybes' xs)
    -- Cheated

-- Small library for Either

{-
lefts' [Left "foo", Right 3, Left "bar", Right 7, Left "baz"] -- ["foo","bar","baz"]
-}
lefts' :: [Either a b] -> [a]
lefts' xs = [ x | Left x <- xs ]
    -- TODO: Rewrite with foldr - looked at a solution... YEAH, RIGHT!

{-
rights' [Left "foo", Right 3, Left "bar", Right 7, Left "baz"] -- [3, 7]
-}
rights' :: [Either a b] -> [b]
rights' xs = [ x | Right x <- xs ]
    -- TODO: Rewrite with foldr - feh

{-
partitionEithers' [Left "foo", Right 3, Left "bar", Right 7, Left "baz"] --
  (["foo","bar","baz"],[3,7])
-}
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

{-
eitherMaybe (*2) (Left "foo") -- Nothing
eitherMaybe (*2) (Right 3)    -- Just 6
-}
eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe _ (Left  _) = Nothing
eitherMaybe f (Right y) = Just (f y)

{-
either' length (*2) (Left "foo") -- 3
either' length (*2) (Right 3)    -- 6
-}
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left  x) = f x
either' _ g (Right y) = g y

{-
eitherMaybe2 (*2) (Left "foo") -- Nothing
eitherMaybe2 (*2) (Right 3)    -- Just 6
-}
eitherMaybe2 :: (b -> c) -> Either a b -> Maybe c
eitherMaybe2 f = either' (const Nothing) (Just . f)
    -- Cheated

-- Unfolds

{-
take 10 $ iterate' (+1) 0 -- [0,1,2,3,4,5,6,7,8,9]
-}
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

{-
take 10 $ unfoldr' (\b -> Just (b, b + 1)) 0 -- [0,1,2,3,4,5,6,7,8,9]
-}
unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' = undefined
