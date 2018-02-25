module Chapter09.Exercises where

import Data.Bool
import Data.Char

-- 9.5 - EnumFromTo

eftBool :: Bool -> Bool -> [Bool]
eftBool from to | from > to  = []
                | from == to = [from]
                | otherwise  = from : eftBool (succ from) to

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to | from > to  = []
               | from == to = [from]
               | otherwise  = from : eftOrd (succ from) to

eftInt :: Int -> Int -> [Int]
eftInt from to | from > to  = []
               | from == to = [from]
               | otherwise  = from : eftInt (succ from) to

eftChar :: Char -> Char -> [Char]
eftChar from to | from > to  = []
                | from == to = [from]
                | otherwise  = from : eftChar (succ from) to

-- 9.6 - Thy Fearful Symmetry

-- 1

{-
myWords "sheryl wants fun" = ["sheryl", "wants", "fun"]
-}
myWords :: String -> [String]
myWords ""      = []
myWords (' ':s) = myWords s
myWords s       = w : myWords t
    where w = takeWhile (/=' ') s
          t = dropWhile (/=' ') s

-- Boy, did I have to cheat on this one. Shit. This seems absurdly difficult for a mid-chepter exercise.

-- 2
-- See PoemLines.hs

-- 3
-- See BreakOn.hs

-- 9.7 - Comprehend Thy Lists

mySqr = [x^2 | x <- [1..10]]

ctl1 = [x | x <- mySqr, rem x 2 == 0]
-- even squares

ctl2 = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- [(1,64),(1,81),(1,100),(4,64),(4,81),(4,100),(9,64),(9,81),(9,100),(16,64),(16,81),(16,100),
-- (25,64),(25,81),(25,100),(36,64),(36,81),(36,100),(49,64),(49,81),(49,100)]

ctl3 = take 5 [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- [(1,64),(1,81),(1,100),(4,64),(4,81)]

-- 9.7 - Square Cube

-- Square Cube

myCube = [y^3 | y <- [1..10]]

sc1 = [(x, y) | x <- mySqr, y <- myCube]

sc2 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

sc3 = length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 9.8 - Bottom Madness

bm01 = [x^y | x <- [1..5], y <- [2, undefined]]             -- blow up
bm02 = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
bm03 = sum [1, undefined, 3]                                -- blow up
bm04 = length [1, 2, undefined]
bm05 = length $ [1, 2, 3] ++ undefined                      -- blow up
bm06 = take 1 $ filter even [1, 2, 3, undefined]
bm07 = take 1 $ filter even [1, 3, undefined]               -- blow up
bm08 = take 1 $ filter odd [1, 3, undefined]
bm09 = take 2 $ filter odd [1, 3, undefined]
bm10 = take 3 $ filter odd [1, 3, undefined]                -- blow up

-- 9.8 - Intermission: Is it in normal form?

{-
1. NF
2. WHNF
3. Neither
4. Neither
5. Neither
6. Neither
7. WHNF
-}

-- 9.9 - More bottoms

mb1 = take 1 $ map (+1) [undefined, 2, 3]       -- bottom
mb2 = take 1 $ map (+1) [1, undefined, 3]       -- value
mb3 = take 2 $ map (+1) [1, undefined, 3]       -- bottom

-- Returns a list of booleans indicating whether each letter in a string is a vowel
itIsMystery :: [Char] -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

mb5a = map (^2) [1..10]                            -- squares of 1 to 10
mb5b = map minimum [[1..10], [10..20], [20..30]]   -- a list of the lowest number in each nested list
mb5c = map sum [[1..5], [1..5], [1..5]]            -- a list of the sum of the items in each nested list

mb6 = map (\x -> bool x (-x) (x == 3)) [1..10]

-- 9.10 - Filtering

f1 = filter (\x -> mod x 3 == 0) [1..30]

f2 = length . filter (\x -> mod x 3 == 0) $ [1..30]

{-
f3 "the brown dog was a goof" = ["brown", "dog", "was", "goof"]
-}
f3 = (filter (\w -> w /= "a" && w /= "an" && w /= "the")) . words

-- 9.11 - Zipping exercises

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y) : zip' (xs) (ys)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f (xs) (ys)

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)

-- 9.12 - Chapter Exercises

-- Data.Char

{-
filterUpper "HbEfLrLxO" = "HELLO"
-}
filterUpper :: [Char] -> [Char]
filterUpper = filter isUpper

{-
capitalize "julie" = "Julie"
-}
capitalize :: [Char] -> [Char]
capitalize ""     = ""
capitalize (x:xs) = toUpper x : xs

{-
allUpper "woot" = "WOOT"
-}
allUpper :: [Char] -> [Char]
allUpper ""     = ""
allUpper (x:xs) = toUpper x : allUpper xs

{-
getCapital "hello" = 'H'
-}
getCapital :: [Char] -> Char
getCapital xs = toUpper $ head xs

getCapital' :: [Char] -> Char
getCapital' xs = (toUpper . head) xs

getCapital'' :: [Char] -> Char
getCapital'' = toUpper . head

-- Ciphers
-- see Cipher.hs

-- 9.12 - Writing your own standard functions

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

or' :: [Bool] -> Bool
or' []       = False
or' (x:xs) = x || or' xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ []     = False
any' f (x:xs) | f x       = True
              | otherwise = any' f xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' e (x:xs) | e == x    = True
               | otherwise = elem' e xs

elem'' :: Eq a => a -> [a] -> Bool
elem'' = \x -> any' (==x)

reverse' :: [a] -> [a]
reverse' = go []
    where go r []     = r
          go r (x:xs) = go (x:r) xs

{-
squish [[1,2],[3,4]] = [1,2,3,4]
-}
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

{-
squishMap (\x -> "WO "++[x]++" HOO ") "123" = "WO 1 HOO WO 2 HOO WO 3 HOO "
-}
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squish' :: [[a]] -> [a]
squish' = squishMap id

{-
maximumBy' compare [1, 53, 9001, 10] = 9001
-}
maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' _ (x:[]) = x
maximumBy' f (x:xs) = case f x y of
                          LT -> y
                          EQ -> x
                          GT -> x
                      where y = maximumBy' f xs
-- I cheated on this one; too hard!
-- How do you even begin to come up with this logic...?!

minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' _ (x:[]) = x
minimumBy' f (x:xs) = case f x y of
                          LT -> x
                          EQ -> x
                          GT -> y
                      where y = minimumBy' f xs

maximum' :: (Ord a) => [a] -> a
maximum' = maximumBy' compare

minimum' :: (Ord a) => [a] -> a
minimum' = minimumBy' compare
