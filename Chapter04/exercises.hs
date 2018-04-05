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
-- not True && True == False
--
-- 2
-- not (5 == 6) == True
--
-- 3
-- OK, but you don't need the parentheses
--
-- 4
-- ["Merry"] > ["Happy"] == True
--
-- 5
-- ['1', '2', '3'] ++ "look at me!" == "123look at me!"
--
--
-- 4.9 - Chapter Exercises
--
awesome = ["Papuchon", "curry", ":)"]

alsoAwesome = ["Quake", "The Simons"]

allAwesome = [awesome, alsoAwesome]

-- 1
-- length :: [a] -> Int
--
-- 2
-- a
-- length [1,2,3,4,5] == 5
--
-- b
-- length [(1,2),(2,3),(3,4)] == 3
--
-- c
-- length allAwesome == 2
--
-- d
-- length (concat allAwesome) == 5
--
-- 3
-- 6 / length [1, 2, 3] returns an error because `length` returns an Int, and Int is not
-- an instance of Fractional which is required for an argument to `/`
--
-- 4
-- I think they're looking for
--      6 `div` length [1, 2, 3] == 2
-- but I believe the better answer is
--      6 / fromIntegral (length [1, 2, 3]) == 2.0
-- because it gives the correct answer for all inputs, not just a numerator that happens
-- to be evenly divisible by the denominator
--
-- 5
-- (2 + 3 == 5) == True
--
-- 6
-- x = 5; (x + 3 == 5) == False
--
-- 7
-- 2, <won't work>, 5, False, <won't work>
--
-- 8
-- isPalindrome "racecar" == True
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9
-- myAbs 1 == 1
-- myAbs (-1) == 1
-- myAbs 0 == 0
myAbs :: Integer -> Integer
myAbs n =
  if n >= 0
    then n
    else negate n

-- 10
-- f (1,'a') (2,'b') == (('a','b'),(1,2))
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- cont. p. 170


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
