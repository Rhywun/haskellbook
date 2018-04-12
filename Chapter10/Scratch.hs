module Scratch where

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

--
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

--
-- 10.9 - Scans
--
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fib :: Int -> Integer
fib = (!!) fibs
