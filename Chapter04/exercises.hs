module Exercises where

--
-- 4.3 - Mood Swing
--
data Mood
  = Blah
  | Woot
  deriving (Show)

-- 1
-- The type constructor is `Mood`
--
-- 2
-- The possible values are `Blah` and `Woot`
--
-- 3
-- changeMood :: Mood -> Woot
-- This is wrong because `Woot` is a value, not a type
--
-- 4, 5
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

--
-- 4.6 - Find the Mistakes
--
-- 1
-- OK
--
-- 2
-- not (x == 6)
--
-- 3
-- OK, but you don't need the parentheses
--
-- 4
-- ["Merry"] > ["Happy"]
--
-- 5
-- ['1', '2', '3'] ++ "look at me!"
--
-- 4.9
awesome = ["Papuchon", "curry", ":)"]

alsoAwesome = ["Quake", "The Simons"]

allAwesome = [awesome, alsoAwesome]

-- 1
-- length :: [a] -> Int
--
-- 2
-- a
-- 5
-- b
-- 3
-- c
-- 2
-- d
-- 5
-- 3
-- 6 / length [1, 2, 3] returns an error because `length` returns an Int, and Int is not an instance
-- of Fractional which is required for an argument to `/`
-- 4
-- I think they're looking for
--      6 `div` length [1, 2, 3]
-- but I believe the better answer is
--      6 / fromIntegral (length [1, 2, 3])
-- because it gives the correct answer for all inputs, not just a numerator that happens to be
-- evenly divisible by the denominator
-- 5
-- Bool, True
-- 6
-- Bool, False
-- 7
-- 2, <won't work>, 5, False, <won't work>
-- 8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9
myAbs :: Integer -> Integer
myAbs x =
  if x >= 0
    then x
    else negate x

-- 10
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

--
-- Correcting syntax
-- 1
x = (+)

f1 xs = w `x` 1
  where
    w = length xs

-- 2
f2 = \x -> x

-- 3
f3 (a, b) = a--
-- Match the function names to their types
-- 1
-- c
-- 2
-- b
-- 3
-- a
-- 4
-- d
