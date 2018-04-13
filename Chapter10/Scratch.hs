module Scratch where

import           Data.Time

--
-- Fold right
--
e01 = map (+ 1) [1, 2, 3]

e02 = foldr (+) 0 [1, 2, 3]
    -- Think of this as replacing the `cons` operator in the list with the (+) function

-- This function returns a True result as soon as it finds one True value,
-- so it does not require the evaluation of every value in the list...
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x b -> f x || b) False

-- ...therefore it can be applied even to an infinite list
{-
myAny even [1 ..] == True
-}
-- Notice we don't hit bottom - even though we `take 4` on a list with only 2 items
fr1 = length $ take 2 $ take 4 xs
  where
    xs = [1, 2] ++ undefined

-- No bottom here either
fr2 = foldr (\_ _ -> 9001) 0 xs
  where
    xs = [undefined, undefined]

--
-- Fold left
--
-- Compare:
--
{-
  Right-associative:
  (1 ^ (2 ^ (3 ^ 2)))
  (1 ^ (2 ^ 9))
  1 ^ 512
  1
-}
e03 = foldr (^) 2 [1 .. 3]

{-
  Left-associative:
  ((2 ^ 1) ^ 2) ^ 3
  (2 ^ 2) ^ 3
  4 ^ 3
  64
-}
e04 = foldl (^) 2 [1 .. 3]

-- Exercises: Understanding Folds
--
uf1 = foldr (*) 1 [1 .. 5] -- 120

-- uf1a = flip (*) 1 [1..5]         -- Syntax error
uf1b = foldl (flip (*)) 1 [1 .. 5] -- 120

uf1c = foldl (*) 1 [1 .. 5] -- 120

uf2 = foldl (flip (*)) 1 [1 .. 3]
  -- (3 * (2 * 1))

-- 3: c
-- 4: a
--
uf5a = foldr (++) "" ["woot", "WOOT", "woot"]

uf5b = foldr max ' ' "fear is the little death"

uf5c = foldr (&&) True [False, True]

uf5d = foldr (||) False [False, True]

uf5e = foldr ((++) . show) "" [1 .. 5]

uf5f = foldr const 'a' ['1' .. '5']

uf5g = foldr const '0' "tacos"

uf5h = foldl (flip const) '0' "burritos"

uf5i = foldl (flip const) 'z' ['1' .. '5']

--
-- 10.6 - How to write fold functions
--
hwff1 = foldr (\a b -> take 3 a ++ b) "" ["Pizza", "Apple", "Banana"]

-- Exercises: Database Processing
--
data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- Again with the cheating. I'm guessing the author wants us to use folds here
-- but I don't see how foldr or foldl applies for these.
{-
filterDbDate theDatabase == [1911-05-01 09:28:43 UTC,1921-05-01 09:28:43 UTC
-}
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = [date | (DbDate date) <- db]

{-
filterDbNumber theDatabase == [9001]
-}
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = [number | (DbNumber number) <- db]

{-
mostRecent theDatabase == 1921-05-01 09:28:43 UTC
-}
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

{-
sumDb theDatabase == 9001
-}
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

{-
avgDb theDatabase == 9001.0
-}
avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sum aFilter) / fromIntegral (length aFilter)
  where
    aFilter = filterDbNumber db

--
-- 10.9 - Scans
--
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fib :: Int -> Integer
fib = (!!) fibs

-- Scans Exercises
--
fibs' :: [Integer]
fibs' = take 20 $ 1 : scanl (+) 1 fibs

fibs'' :: [Integer]
fibs'' = filter (< 100) (1 : scanl (+) 1 fibs)

factorials = take 5 $ scanl (*) 1 [2 ..]
