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
f :: a -> a -> a -> a; f = undefined
x :: Char; x = undefined
-- Char -> Char -> Char

-- 2
g :: a -> b -> c -> b; g = undefined
-- Char

-- 3
h :: (Num a, Num b) => a -> b -> b; h = undefined
-- Num b => b

-- 4
-- Double

-- 5
jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
-- [Char]

-- 6
-- Eq b => b -> [Char]

-- 7
kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
-- Num a => a -- wrong
-- (Num a, Ord a) => a, why? I think because (a) we know it's an Ord, and the 1 tells us
--   it's a Num, and there's no inhertance relationship between Num and Ord

-- 8
-- (Num a, Ord a) => a, no change from previous

-- 9
-- Integer

--
-- 5.5 - Parametricity

-- 1
{-
f1 :: a -> a
f1 x = negate x
-- Error: No instance for (Num a) arising from a use of ‘+’/'^'/'negate' etc.
-}

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
-- 5.6 - Apply Yourself

myConcat x = x ++ " yo"
-- myConcat :: [Char] -> [Char]

myMult x = (x / 3) * 5
-- myMult :: Fractional a => a -> a

myTake x = take x "hey you"
-- myTake :: Int -> [Char]

myCom x = x > (length [1..10])
-- myCom :: Int -> Bool

--
-- 5.8 - Chapter Exercises

-- Multiple choice

{-
1c, 2a, 3b, 4c
-}

-- Determine the type - see separate file

-- Does it compile?

bigNum = (^) 5 $ 10
wahoo = bigNum

x1 = print
y = print "woohoo!"
z = x1 "hello world"

a = (+)
b = 5
c = a 10
d = c 200

a4 = 12 + b4
b4 = 10000

-- Type variable or specific type constructor?

{-
2. fully polymorphic, concrete, concrete
3. fully polymorphic, constrained polymorphic, concrete
4. fully polymorphic, fully polymorphic, concrete
-}

-- Write a type signature

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function

i :: a -> a
i x = x

c1 :: a -> b -> a
c1 x _ = x

c'' :: b -> a -> b
c'' x _ = x

c' :: a -> b -> b
c' _ y = y

r :: [a] -> [a]
r (x:xs) = xs

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = (bToC (aToB a))

a1 :: (a -> c) -> a -> a
a1 _ x = x

a' :: (a -> b) -> a -> b
a' aToB a = (aToB a)

{-
-- Fix it
-- See Sing.hs, Arith3Broken.hs

-- Type-Kwon-Do
-- See TypeKwonDo.hs
-}