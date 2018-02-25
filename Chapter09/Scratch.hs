module Chapter09.Scratch where

import Data.Bool

-- 9.3 - Pattern matching on lists

myTail :: [a] -> [a]
myTail []     = []
myTail (_:xs) = xs

-- I'm not clear on why this is "better", at least for the 2nd case
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Nothing       -- shouldn't this one return []?
safeTail (_:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- 9.7 - List comprehensions

lc1 = [x^2 | x <- [1..10]]

lc2 = [x^2 | x <- [1..10], rem x 2 == 0]

lc3 = [x^y | x <- [1..10], y <- [2, 3]]

lc4 = [x^y | x <- [1..10], y <- [2, 3], x^y < 200]

lc5 = [(x, y) | x <- [1, 2, 3], y <- [6, 7]]

lc6 = [(x, y) | x <- [1, 2, 3], y <- ['a', 'b']]

lc7 = [(x, y) | x <- squares, y <- [1..3], x < 10] where squares = [x^2 | x <- [1..10]]

chess = [x:y:[] | x <- ['a'..'h'], y <- ['1'..'8']]

{-
acro "Three Letter Acronym" = "TLA"
-}
acro xs = [x | x <- xs, elem x ['A'..'Z']]

{-
myString "Mississippi" = "iiii"
-}
myString xs = [x | x <- xs, elem x "aeiou"]

-- 9.8 - Spines

-- Forces spine only
length' :: [a] -> Integer
length' []     = 0
length' (_:xs) = 1 + length' xs

-- Forces spine and values
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 9.9 - Transforming lists of values

tlv1 = map (take 3) [[1..5], [1..5], [1..5]]
tlv2 = map (\x -> if x == 3 then (-x) else (x)) [1..10]

-- 9.11 - Zipping lists

z1 = zipWith (+) [1, 2, 3] [4, 5, 6]
