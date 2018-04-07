{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- Without the NoMonomorphismRestriction language extension, this
-- defaults to :: Integer instead of :: Num p => p
example = 1

-- 1
--
-- a
e1a = (* 9) 6 -- 54 :: Num a => a

-- b
e1b = head [(0, "doge"), (1, "kitteh")] -- (0,"doge") :: Num a => (a, [Char])

-- c
e1c = head [(0 :: Integer, "doge"), (1, "kitteh")] -- (0,"doge") :: (Integer, [Char])

-- d
e1d =
  if False
    then True
    else False -- False :: Bool

-- e
e1e = length [1, 2, 3, 4, 5] -- 5 :: Int

-- f
e1f = length [1, 2, 3, 4] > length "TACOCAT" -- False :: Bool

-- 2
--
x2 = 5

y2 = x2 + 5

w2 = y2 * 10 -- w2 :: Num a => a

-- 3
--
x3 = 5

y3 = x3 + 5

z3 y3 = y3 * 10 -- z3 :: Num a => a -> a

-- 4
--
x4 = 5

y4 = x4 + 5

f4 = 4 / y4 -- f4 :: Fractional a => a

-- 5
--
x5 = "Julie"

y5 = " <3 "

z5 = "Haskell"

f5 = x5 ++ y5 ++ z5 -- f5 :: [Char]
