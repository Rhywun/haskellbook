module Chapter05.Scratch where

--
-- 5.3 - How to read type signatures
--
-- This won't compile because `a` does not have an instance of Num, as
-- required by the use of (+)
{-
fst' :: (a, b) -> a
fst' (x, y) = x + 1
-}
--
-- 5.4 - Currying
--
addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

subtractStuff :: Integer -> Integer -> Integer
subtractStuff x y = x - y - 10

subtract1 = subtractStuff 1

nonsense :: Bool -> Integer
nonsense True  = 805
nonsense False = 31337

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + nonsense b

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + nonsense b

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + nonsense b

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + nonsense b

-- Currying and uncurrying
--
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

-- Sectioning
--
x = 5

y = (2 ^)

z = (^ 2)

celebrate = (++ " woot!")

-- difference between infix...
isInRange = (`elem` [1 .. 10])

-- ...and prefix forms
hasTen :: (Eq a, Num a) => [a] -> Bool -- for some reason this is required
                                       -- in a file but not in the REPL
hasTen = elem 10

--
-- 5.5 - Polymorphism
--
-- No instance for (Fractional Int) arising from a use of ‘/’
-- l' = 6 / length [1, 2, 3]
l = 6 / fromIntegral (length [1, 2, 3])

--
-- 5.6 - Type inference
--
myGreet x = x ++ " Julie" -- [Char] -> [Char]

myGreet' x y = x ++ y -- [a] -> [a] -> [a]

--
-- 5.7 - Asserting types for declarations
--
triple'' x = x * 3 -- :: Num a => a -> a

triple' x = x * 3 :: Integer -- :: Integer -> Integer

triple :: Integer -> Integer
triple x = x * 3

-- Uncommon
triple2 = tripleItYo -- Int -> Int
  where
    tripleItYo :: Int -> Int
    tripleItYo y = y * 3
