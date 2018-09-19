module Chapter10.Scratch where

import           Data.Time

--
-- 10.4 - Fold right
--

e01 = map (+ 1) [1, 2, 3]

e02 = foldr (+) 0 [1, 2, 3]
    -- Think of this as replacing the `cons` operator in the list with the (+) function

-- This function returns a True result as soon as it finds one True value,
-- so it does not require the evaluation of every value in the list...
-- ...therefore it can be applied even to an infinite list
{-
myAny even [1 ..] -- True
-}
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x b -> f x || b) False

-- Notice we don't hit bottom - even though we `take 4` on a list with only 2 items
fr1 = length $ take 2 $ take 4 xs where xs = [1, 2] ++ undefined -- 2

-- No bottom here either
fr2 = foldr (\_ _ -> 9001) 0 xs where xs = [undefined, undefined] -- 9001

-- Or here
fr3 = foldr (+) 0 xs where xs = take 4 [1, 2, 3, 4, undefined] -- 10

-- Or here - `const` throws away the 2nd argument ("the rest of the fold")
fr4 = foldr const 0 [1, undefined] -- 1

--
-- 10.5 - Fold left
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
fl1 = foldr (^) 2 [1 .. 3]

{-
  Left-associative:
  ((2 ^ 1) ^ 2) ^ 3
  (2 ^ 2) ^ 3
  4 ^ 3
  64
-}
fl2 = foldl (^) 2 [1 .. 3]

-- Exercises: Understanding Folds

-- 1

uf1 = foldr (*) 1 [1 .. 5] -- 120

-- uf1a = flip (*) 1 [1..5]         -- Syntax error
uf1b = foldl (flip (*)) 1 [1 .. 5] -- 120
uf1c = foldl (*) 1 [1 .. 5] -- 120

uf2 = foldl (flip (*)) 1 [1 .. 3]
  -- (3 * (2 * 1))

-- 2

{-
foldl (flip (*)) 1 [1..3]
== ((1 * 1) * 2) * 3
== (1 * 2) * 3
== 2 * 3
== 6
-}

-- 3: c

-- 4: a

-- 5

uf5a = foldr (++) "" ["woot", "WOOT", "woot"] -- "wootWOOTwoot"

uf5b = foldr max ' ' "fear is the little death" -- 't'

uf5c = foldr (&&) True [False, True] -- False

uf5d = foldr (||) False [False, True] -- True

uf5e = foldr ((++) . show) "" [1 .. 5] -- "12345"

uf5f = foldr const 'a' ['1' .. '5'] -- '1'

uf5g = foldr const '0' "tacos" -- 't'

uf5h = foldl (flip const) '0' "burritos" -- 's'

uf5i = foldl (flip const) 'z' ['1' .. '5'] -- '5'

--
-- 10.6 - How to write fold functions
--

hwff1 = foldr (\a b -> take 3 a ++ b) "" ["Pizza", "Apple", "Banana"] -- "PizAppBan"

-- Exercises: Database Processing

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate   UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- Again with the cheating. I thought the author wants us to use folds here
-- but I don't see how foldr or foldl applies for these.

{-
filterDbDate theDatabase -- [1911-05-01 09:28:43 UTC,1921-05-01 09:28:43 UTC
-}
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = [ date | (DbDate date) <- db ]

{-
filterDbNumber theDatabase -- [9001]
-}
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = [ number | (DbNumber number) <- db ]

{-
mostRecent theDatabase -- 1921-05-01 09:28:43 UTC
-}
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

{-
sumDb theDatabase -- 9001
-}
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

{-
avgDb theDatabase -- 9001.0
-}
avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sum aFilter) / fromIntegral (length aFilter)
  where aFilter = filterDbNumber db

--
-- 10.9 - Scans
--

-- WARN: Infinite list!
{-
take 10 fibs -- [1,1,2,3,5,8,13,21,34,55]
-}
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

{-
fib 10 -- 89
-}
fib :: Int -> Integer
fib = (fibs !!)

-- Scans Exercises

{-
fibs' -- [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]
-}
fibs' :: [Integer]
fibs' = take 20 $ 1 : scanl (+) 1 fibs'

{-
fibs'' -- [1,1,2,3,5,8,13,21,34,55,89]
-}
fibs'' :: [Integer]
fibs'' = takeWhile (< 100) (1 : scanl (+) 1 fibs'')

{-
factorials 5 -- [1,2,6,24,120]
-}
factorials :: Int -> [Int]
factorials n = take n $ scanl (*) 1 [2 ..]
