module Exercises where

--
-- 7.3 - Grab Bag
--
-- 1
-- all are equivalent
mth1 x y z = x * y * z

mth2 x y = \z -> x * y * z

mth3 x = \y -> \z -> x * y * z

mth4 = \x -> \y -> \z -> x * y * z
  -- Q: Why is this :: Integer -> Integer -> Integer -> Integer ??
  -- A: I think because of the way Num sometimes defaults to Integer.
  --    Apparently, when all parameters are inside the anonymous function is
  --    one of those times.

-- 2
-- Num a => a -> a -> a
--
-- 3
addOne = \x -> x + 1

-- a
addOneIfOdd n =
  case odd n of
    True  -> (\x -> x + 1) n
    False -> n

-- b
addFive =
  \x ->
    \y ->
      (if x > y
         then y
         else x) +
      5

-- c
mflip f x y = f y x

--
-- 7.4 - Variety Pack
--
k (x, _) = x

k1 = k (4 - 1, 10)

k2 = k ("three", 1 + 2)

k3 = k (3, True)

-- 1
-- a - k :: (a -> b) -> a
--
-- b - k2 :: [Char] - No, it is different from the types of k1 and k3
--
-- c - k3 == 3
--
-- 2
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

--
-- 7.5 - Case Practice
--
-- 1
functionC x y =
  case x > y of
    True  -> x
    False -> y

-- 2
ifEvenAdd2 n =
  case even n of
    True  -> n + 2
    False -> n

-- 3
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

--
-- 7.6 - Artful Dodgy
--
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = flip dodgy 2

{-
  dodgy 1 0 = 1
  dodgy 1 1 = 11
  dodgy 2 2 = 22
  dodgy 2 1 = 12
  oneIsOne 1 = 11
  oneIsOne 2 = 21
  oneIsTwo 1 = 21
  oneIsTwo 2 = 22
  oneIsOne 3 = 31
  oneIsTwo 3 = 23
-}
--
--
-- 7.7 - Guard Duty
--
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | otherwise = 'F'
  | y >= 0.7 = 'C'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'E'
  where
    y = x / 100
    -- 1: All grades are 'F'
    -- 2: Depending on the order of the clauses, the returned grade can be wrong

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False
    -- 3: b, True when xs is a palindrome
    -- 4: A list of any type that has an instance of Eq
    -- 5: Eq a => [a] -> Bool

numbers :: (Ord a, Num a, Num t) => a -> t
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1
    -- 6: c, an indication of whether its argument is a positive or negative number or zero
    -- 7: Any type that has an instance of Ord and Num
    -- 8: (Ord a, Num a, Num t) => a -> t

--
-- 7.11 - Chapter Exercises
--
-- Multiple choice
-- 1 -> d
-- 2 -> b
-- 3 -> d
-- 4 -> b
-- 5 -> a
--
-- Let's write code
-- 1
tensDigit :: Integral a => a -> a
tensDigit x = mod (fst (divMod x 10)) 10

hunsDigit :: Integral a => a -> a
hunsDigit x = mod (fst (divMod x 100)) 10

-- 2
foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True  -> y
    False -> x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b == False = x
  | otherwise = y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (x, z) = (f x, z)
-- 4, 5, 6
-- See Arith4.hs
