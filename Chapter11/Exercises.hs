module Exercises where

import Data.Char

-- Multiple choice

data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday

-- 1. a) Weekday is a type with five data constructors

f Friday = "Miller Time"

-- 2. c) f :: Weekday -> String

-- 3. b) must begin with a capital letter

g xs = xs !! (length xs - 1)

-- 4. c) delivers the final element of xs

-- Ciphers
-- see Ciphers.hs

-- As-patterns

f' :: Show a => (a, b) -> IO (a, b)
f' t@(a, _) = do
    print a
    return t

doubleUp :: [a] -> [a]
doubleUp []       = []
doubleUp xs@(x:_) = x : xs

isSubSeqOf :: (Eq a) => [a] -> [a] -> Bool
isSubSeqOf = undefined
-- Pass

{-
capitalizeWords "hello world" = [("hello", "Hello"), ("world", "World")]
-}
-- capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = [(x, (toUpper $ head x) : tail x) | x <- words xs]
-- No idea how to do this with `@`

-- Language exercises

capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (x:xs) = toUpper x : xs

{-
capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph ws = [w | w <- words ws]

s = "blah. woot ha."
-}
-- I give up.

-- Phone exercise
-- LOL pass

-- Hutton's Razor

data Expr = Lit Integer | Add Expr Expr

{-
eval (Add (Lit 1) (Lit 2)) = 3
-}
eval :: Expr -> Integer
eval (Lit n) = n
eval (Add x y) = eval x + eval y

{-
printExpr (Add (Lit 1) (Lit 2)) = "1 + 2"
-}
printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y

