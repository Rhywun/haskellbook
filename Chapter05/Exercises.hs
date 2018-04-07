module Exercises where

--
-- 5.3 - Type Matching
{-
head :: [a] -> a
concat :: [[a]] -> [a]
not :: Bool -> Bool
length :: [a] -> Int
(<) :: Ord a => a -> a -> Bool
-}
--
-- 5.4 - Type Arguments
-- 1
f :: a -> a -> a -> a
f = undefined

x :: Char
x = undefined

-- f x :: Char -> Char -> Char
--
-- 2
g :: a -> b -> c -> b
g = undefined

-- g 0 'c' "woot" :: Char
--
-- 3
h :: (Num a, Num b) => a -> b -> b
h = undefined

-- h 1.0 2 :: Num b => b
--
-- 4
-- h 1 (5.5 :: Double) :: Double
--
-- 5
jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined

-- jackal "keyboard" "has the word jackal in it" :: [Char]
--
-- 6
-- jackal "keyboard" :: Eq b => b -> [Char]
--
-- 7
kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined

-- kessel 1 2 :: (Num a, Ord a) => a
--
-- 8
-- kessel 1 (2 :: Integer) :: (Num a, Ord a) => a
--
-- 9
-- kessel (1 :: Integer) 2 :: Integer
--
--
-- 5.5 - Parametricity
-- 1
{-
f1 :: a -> a
f1 x = negate x
-- Error: No instance for (Num a) arising from a use of ‘+’/'^'/'negate' etc.
-}
--
-- 2
f2 :: a -> a -> a
f2 x y = x

f2' :: a -> a -> a
f2' x y = y

{-
f2'' :: a -> a -> a
f2'' x y = x + y
-- No instance for (Num a) arising from a use of ‘+’
-}
--
-- 3
{-
f3 :: a -> b -> b
f3 x y = x
-- Couldn't match expected type ‘b’ with actual type ‘a’
-}
f3' :: a -> b -> b
f3' x y = y

-- One and only one implementation
-- Behavior is the same for all types of a and b
--
--
-- 5.6 - Apply Yourself
--
-- 1
myConcat x = x ++ " yo" -- :: [Char] -> [Char]

-- 2
myMult x = (x / 3) * 5 -- :: Fractional a => a -> a

-- 3
myTake x = take x "hey you" -- :: Int -> [Char]

-- 4
myCom x = x > length [1 .. 10] -- :: Int -> Bool

--
--
-- 5.8 - Chapter Exercises
--
-- Multiple choice
{-
1c, 2a, 3b, 4c
-}
--
-- Determine the type
-- see DetermineTheType.hs
--
--
-- Does it compile?
--
-- 1
--
bigNum = (^) 5 $ 10

-- wahoo = bigNum $ 10
wahoo = bigNum

-- 2
--
x2 = print

y2 = print "woohoo!"

z2 = x2 "hello world"

-- 3
--
a3 = (+)

b3 = 5

c3 = a3 10

d3 = c3 200

-- 4
--
a4 = 12 + b4

b4 = 10000

--
-- Type variable or specific type constructor?
--
{-
2. fully polymorphic, concrete, concrete
3. fully polymorphic, constrained polymorphic, concrete
4. fully polymorphic, fully polymorphic, concrete
-}
--
-- Write a type signature
--
-- 1
functionH :: [a] -> a
functionH (x:_) = x

-- 2
functionC :: Ord a => a -> a -> Bool
functionC x y =
  if x > y
    then True
    else False

-- 3
functionS :: (a, b) -> b
functionS (x, y) = y

--
-- Given a type, write the function
--
-- 1
i :: a -> a
i x = x

-- 2
c :: a -> b -> a
c x _ = x

-- 3
-- Yes, same as `c`
c'' :: b -> a -> b
c'' x _ = x

-- 4
c' :: a -> b -> b
c' _ y = y

-- 5
r :: [a] -> [a]
r (x:xs) = xs

-- 6
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

-- 7
a1 :: (a -> c) -> a -> a
a1 _ x = x

-- 8
a' :: (a -> b) -> a -> b
a' aToB = aToB
--
--
-- Fix it
-- See Sing.hs, Arith3Broken.hs
--
-- Type-Kwon-Do
-- See TypeKwonDo.hs
