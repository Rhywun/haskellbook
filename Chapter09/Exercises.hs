module Exercises where

import           Data.Bool
import           Data.Char

--
-- Data.Char
--
{-
filterUpper "HbEfLrLxO" == "HELLO"
-}
filterUpper :: String -> String
filterUpper = filter isUpper

{-
capitalize "julie" == "Julie"
-}
capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : xs

{-
allUpper "woot" == "WOOT"
-}
allUpper :: String -> String
allUpper ""     = ""
allUpper (x:xs) = toUpper x : allUpper xs

{-
getCapital'' "hello" == 'H'
-}
getCapital :: String -> Char
getCapital xs = toUpper $ head xs

getCapital' :: String -> Char
getCapital' = toUpper . head

getCapital'' :: String -> Char
getCapital'' = toUpper . head

--
-- Ciphers
-- see Cipher.hs
--
--
-- Writing your own standard functions
--
and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

or' :: [Bool] -> Bool
or' []     = False
or' (x:xs) = x || or' xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs)
  | f x = True
  | otherwise = any' f xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
  | e == x = True
  | otherwise = elem' e xs

elem'' :: Eq a => a -> [a] -> Bool
elem'' x = any' (== x)

reverse' :: [a] -> [a]
reverse' = go []
  where
    go r []     = r
    go r (x:xs) = go (x : r) xs

{-
squish [[1,2],[3,4]] == [1,2,3,4]
-}
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

{-
squishMap (\x -> "WO " ++ [x] ++ " HOO ") "123" == "WO 1 HOO WO 2 HOO WO 3 HOO "
-}
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

squish' :: [[a]] -> [a]
squish' = squishMap id

{-
maximumBy' compare [1, 53, 9001, 10] == 9001
-}
maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _ [x] = x
maximumBy' f (x:xs) =
  case f x y of
    LT -> y
    EQ -> x
    GT -> x
  where
    y = maximumBy' f xs

minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' _ [x] = x
minimumBy' f (x:xs) =
  case f x y of
    LT -> x
    EQ -> x
    GT -> y
  where
    y = minimumBy' f xs

maximum' :: (Ord a) => [a] -> a
maximum' = maximumBy' compare

minimum' :: (Ord a) => [a] -> a
minimum' = minimumBy' compare
