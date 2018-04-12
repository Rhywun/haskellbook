module Scratch where

import           Data.Bool

--
-- 9.3 - Pattern matching on lists
--
myTail :: [a] -> [a]
myTail []     = []
myTail (_:xs) = xs

-- I'm not clear on why this is "better", at least for the 2nd case
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail [x]    = Nothing -- shouldn't this one return []?
safeTail (_:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

--
-- 9.5 - Using ranges to construct lists
--
--
-- Exercise: EnumFromTo
--
eftBool :: Bool -> Bool -> [Bool]
eftBool from to
  | from > to = []
  | from == to = [from]
  | otherwise = from : eftBool (succ from) to

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to
  | from > to = []
  | from == to = [from]
  | otherwise = from : eftOrd (succ from) to

eftInt :: Int -> Int -> [Int]
eftInt from to
  | from > to = []
  | from == to = [from]
  | otherwise = from : eftInt (succ from) to

eftChar :: Char -> Char -> String
eftChar from to
  | from > to = []
  | from == to = [from]
  | otherwise = from : eftChar (succ from) to

--
-- 9.6 - Extracting portions of lists
--
-- Exercises: Thy Fearful Symmetry
--
-- 1
{-
myWords "sheryl wants fun" == ["sheryl", "wants", "fun"]
-}
myWords :: String -> [String]
myWords "" = []
myWords (' ':s) = myWords s
myWords s = w : myWords t
  where
    w = takeWhile (/= ' ') s
    t = dropWhile (/= ' ') s
    -- Boy, did I have to cheat on this one. Shit. This seems absurdly difficult for a
    -- mid-chepter exercise.

--
-- 2
-- See PoemLines.hs
--
-- 3
-- See BreakOn.hs
--
--
-- 9.7 - List comprehensions
--
lc1 = [x ^ 2 | x <- [1 .. 10]] -- [1,4,9,16,25,36,49,64,81,100]

lc2 = [x ^ 2 | x <- [1 .. 10], rem x 2 == 0]
    -- [4,16,36,64,100]

lc3 = [x ^ y | x <- [1 .. 10], y <- [2, 3]]
    -- [1,1,4,8,9,27,16,64,25,125,36,216,49,343,64,512,81,729,100,1000]

lc4 = [x ^ y | x <- [1 .. 10], y <- [2, 3], x ^ y < 200]
    -- [1,1,4,8,9,27,16,64,25,125,36,49,64,81,100]

lc5 = [(x, y) | x <- [1, 2, 3], y <- [6, 7]]
    -- [(1,6),(1,7),(2,6),(2,7),(3,6),(3,7)]

lc6 = [(x, y) | x <- [1, 2, 3], y <- ['a', 'b']]
    -- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]

lc7 = [(x, y) | x <- squares, y <- [1 .. 3], x < 10]
  where
    squares = [x ^ 2 | x <- [1 .. 10]]
    -- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]

--
-- Exercises - Comprehend Thy Lists
--
mySqr = [x ^ 2 | x <- [1 .. 10]] -- [1,4,9,16,25,36,49,64,81,100]

ctl1 = [x | x <- mySqr, rem x 2 == 0]
    -- [4,16,36,64,100]

ctl2 = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
    -- [(1,64),(1,81),(1,100),(4,64),(4,81),(4,100),(9,64),(9,81),
    --  (9,100),(16,64),(16,81),16,100),(25,64),(25,81),(25,100),
    --  (36,64),(36,81),(36,100),(49,64),(49,81),(49,100)]

ctl3 = take 5 [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
    -- [(1,64),(1,81),(1,100),(4,64),(4,81)]

--
chess = [[x, y] | x <- ['a' .. 'h'], y <- ['1' .. '8']]

{-
acro "Three Letter Acronym" == "TLA"
-}
acro xs = [x | x <- xs, x `elem` ['A' .. 'Z']]

{-
myString "Mississippi" == "iiii"
-}
myString xs = [x | x <- xs, x `elem` "aeiou"]

--
-- Exercises: Square Cube
--
myCube = [y ^ 3 | y <- [1 .. 10]] -- [1,8,27,64,125,216,343,512,729,1000]

sc1 = [(x, y) | x <- mySqr, y <- myCube]

sc2 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

sc3 = length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
    -- 21

--
-- 9.8 - Spines and nonstrict evaluation
--
-- Forces spine only
length' :: [a] -> Integer
length' []     = 0
length' (_:xs) = 1 + length' xs

-- Forces spine and values
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- Exercises - Bottom Madness
bm01 = [x ^ y | x <- [1 .. 5], y <- [2, undefined]] -- blow up

bm02 = take 1 [x ^ y | x <- [1 .. 5], y <- [2, undefined]]

bm03 = sum [1, undefined, 3] -- blow up

bm04 = length [1, 2, undefined]

bm05 = length $ [1, 2, 3] ++ undefined -- blow up

bm06 = take 1 $ filter even [1, 2, 3, undefined]

bm07 = take 1 $ filter even [1, 3, undefined] -- blow up

bm08 = take 1 $ filter odd [1, 3, undefined]

bm09 = take 2 $ filter odd [1, 3, undefined]

bm10 = take 3 $ filter odd [1, 3, undefined] -- blow up

-- Intermission: Is it in normal form?
{-
1. NF
2. WHNF
3. Neither
4. Neither
5. Neither
6. Neither
7. WHNF
-}
--
--
-- 9.9 - Transforming lists of values
--
tlv1 = map (take 3) [[1 .. 5], [1 .. 5], [1 .. 5]] -- [[1,2,3],[1,2,3],[1,2,3]]

tlv2 =
  map
    (\x ->
       if x == 3
         then (-x)
         else x)
    [1 .. 10]
    -- [1,2,-3,4,5,6,7,8,9,10]

--
-- Exercises: More bottoms
--
mb1 = take 1 $ map (+ 1) [undefined, 2, 3] -- bottom

mb2 = take 1 $ map (+ 1) [1, undefined, 3] -- value

mb3 = take 2 $ map (+ 1) [1, undefined, 3] -- bottom

-- Returns a list of booleans indicating whether each letter in a string is a vowel
{-
itIsMystery "hello" == [False,True,False,False,True]
-}
itIsMystery :: String -> [Bool]
itIsMystery = map (`elem` "aeiou")

mb5a = map (^ 2) [1 .. 10]
    -- squares of 1 to 10

mb5b = map minimum [[1 .. 10], [10 .. 20], [20 .. 30]]
    -- a list of the lowest number in each nested list

mb5c = map sum [[1 .. 5], [1 .. 5], [1 .. 5]]
    -- a list of the sum of the items in each nested list

mb6 = map (\x -> bool x (-x) (x == 3)) [1 .. 10]
    -- [1,2,-3,4,5,6,7,8,9,10]

--
-- Filtering lists of values
--
-- Exercises: Filtering
--
f1 = filter (\x -> mod x 3 == 0) [1 .. 30] -- [3,6,9,12,15,18,21,24,27,30]

f2 = length . filter (\x -> mod x 3 == 0) $ [1 .. 30]
    -- 10

{-
f3 "the brown dog was a goof" == ["brown", "dog", "was", "goof"]
-}
f3 = filter (\w -> w /= "a" && w /= "an" && w /= "the") . words

--
-- 9.11 - Zipping lists
--
z1 = zipWith (+) [1, 2, 3] [4, 5, 6] -- [5,7,9]

--
-- Zipping exercises
--
{-
zip' [1,2,3] [4,5,6] == [(1,4),(2,5),(3,6)]
-}
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

{-
zipWith' (+) [1, 2, 3] [4, 5, 6] == [5,7,9]
-}
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

{-
zip'' [1,2,3] [4,5,6] == [(1,4),(2,5),(3,6)]
-}
zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)
