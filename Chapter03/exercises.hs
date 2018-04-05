module Exercises where

-- 3.4 - Scope
--
-- 1
-- Yes
--
-- 2
-- No
--
-- 3
-- area d = pi * (r * r)
-- r = d / 2
-- No, the 2nd d is not in scope
--
-- 4
area' d = pi * (r * r)
  where
    r = d / 2

-- Yes
--
--
-- 3.5 - Syntax Errors
--
-- 1
-- No
e030501 = (++) [1, 2, 3] [4, 5, 6]

--
-- 2
-- No
e030502 = "<3" ++ "Haskell"

--
-- 3
-- Yes
e030503 = concat ["<3", "Haskell"]

--
-- 3.8 - Chapter Exercises
--
-- Reading syntax
--
-- 1
-- a
-- Yes
--
-- b
-- No
-- (++) [1, 2, 3] [4, 5, 6]
--
-- c
-- Yes
--
-- d
-- No
-- ["hello" ++ " world"]
--
-- e
-- No
-- "hello" !! 4
--
-- f
-- Yes
--
-- g
-- No
-- take 4 "lovely"
--
-- h
-- Yes
--
-- 2
-- a
-- concat [[1 * 6], [2 * 6], [3 * 6]] == [6,12,18]
--
-- b
-- "rain" ++ drop 2 "elbow" == "rainbow"
--
-- c
-- 10 * head [1, 2, 3] == 10
--
-- d
-- (take 3 "Julie") ++ (tail "yes") == "Jules"
--
-- e
-- concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]] == [2,3,5,6,8,9]
--
--
-- Building functions
--
-- 1
-- a
-- "Curry is awesome" ++ "!" == "Curry is awesome!"
--
-- b
-- drop 4 (take 5 "Curry is awesome!") == "y"
--
-- c
-- drop 9 "Curry is awesome!" == "awesome!"
--
-- 2
-- a
f1 x = x ++ "!"

-- b
f2 x = drop 4 (take 5 x)

-- c
f3 = drop 9

-- 3
thirdLetter :: String -> Char
thirdLetter x = x !! 2

-- 4
letterIndex :: Int -> Char
letterIndex n = "Curry is awesome!" !! (n - 1)

-- 5
reverse' = a ++ " " ++ i ++ " " ++ c
  where
    s = "Curry is awesome"
    c = take 5 s
    i = drop 6 (take 8 s)
    a = drop 9 s--
--
-- 6
-- see Reverse.hs
