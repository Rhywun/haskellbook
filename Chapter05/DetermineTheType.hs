{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

example = 1

e1a = (* 9) 6
-- 54 :: Num a => a

e1b = head [(0,"doge"),(1,"kitteh")]
-- (0,"doge") :: Num a => (a, [Char])

e1c = head [(0 :: Integer ,"doge"),(1,"kitteh")]
-- (0,"doge") :: (Integer, [Char])

e1d = if False then True else False
-- False :: Bool

e1e = length [1, 2, 3, 4, 5]
-- 5 :: Int

e1f = (length [1, 2, 3, 4]) > (length "TACOCAT")
-- False :: Bool

{-
x = 5
y = x + 5
w = y * 10
-- w :: Num a => a
-}

{-
x = 5
y = x + 5
z y = y * 10
-- z :: Num a => a -> a
-}

{-
x = 5
y = x + 5
f = 4 / y
-- f :: Fractional a => a
-}

x = "Julie"
y = " <3 "
z = "Haskell"
f = x ++ y ++ z
-- f :: [a] -- wrong
-- f :: [Char] because the use of strings narrows the type down from `a`
