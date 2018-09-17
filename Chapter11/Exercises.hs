module Chapter11.Exercises where

import           Data.Char

-- Multiple choice
--
data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

--
-- 1. a) Weekday is a type with five data constructors
--
f Friday = "Miller Time"

-- 2. c) f :: Weekday -> String
-- 3. b) must begin with a capital letter
--
g xs = xs !! (length xs - 1)

-- 4. c) delivers the final element of xs
--
-- Ciphers
-- see Ciphers.hs
--
-- As-patterns
--
-- Prints the first item of a tuple and returns the whole tuple
f' :: Show a => (a, b) -> IO (a, b)
f' t@(a, _) = do
  print a
  return t

-- Returns a list with the first item doubled
doubleUp :: [a] -> [a]
doubleUp []       = []
doubleUp xs@(x:_) = x : xs

-- 1
{-
isSubSeqOf "blah" "wboloath" == True
isSubSeqOf "blah" "halbwoot" == False
-}
isSubSeqOf :: Eq a => [a] -> [a] -> Bool
isSubSeqOf [] _ = True
isSubSeqOf _ [] = False
isSubSeqOf (x:xs) phrase@(y:ys) =
  if x `elem` phrase
    then isSubSeqOf xs ys
    else isSubSeqOf (x : xs) ys

-- Cheated, but I was on the right track...
{-
loop through "blah":
'b' `elem` "wboloath"? yes ->
'l' `elem`   "oloath"? yes ->
'a' `elem`     "oath"? yes ->
'h' `elem`       "th"? yes -> True

loop through "blah":
'b' `elem` "halbwoot"? yes ->
'l' `elem`     "woot"? no  -> False
-}
--
-- 2
{-
capitalizeWords "hello world" == [("hello", "Hello"), ("world", "World")]
-}
capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = [(x, toUpper (head x) : tail x) | x <- words xs]
    -- No idea how to do this with `@`

-- Language exercises
--
-- 1
{-
capitalizeWord "Chortle" == "Chortle"
capitalizeWord "chortle" == "Chortle"
-}
capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (x:xs) = toUpper x : xs

-- 2
--
capitalizeParagraph :: String -> String
capitalizeParagraph para = go (capitalizeWord para)
  where
    go []           = []
    go ('.':' ':xs) = ". " ++ go (capitalizeWord xs)
    go (x:xs)       = x : go xs

-- Cheated, but I was on the right track...
{-
capitalizeParagraph [] = []
capitalizeParagraph xs = go (words xs) True
  where
    go (w:ws) True = capitalizeWord w ++ go ws (p w)
    go (w:ws) False = w ++ go ws (p w)
    go w True = capitalizeWord w
    go w False = w
    p w = last w == '.'
-}
{-
capitalizeParagraph s == "Blah. Woot ha."
-}
s = "blah. woot ha."

-- Phone exercise
-- see Phone.hs
--
-- Hutton's Razor
--
data Expr
  = Lit Integer
  | Add Expr
        Expr

{-
eval (Add (Lit 1) (Lit 2)) == 3
-}
eval :: Expr -> Integer
eval (Lit n)   = n
eval (Add x y) = eval x + eval y

{-
printExpr (Add (Lit 1) (Lit 2)) == "1 + 2"
-}
printExpr :: Expr -> String
printExpr (Lit n)   = show n
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
