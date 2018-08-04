module Chapter03.Exercises where

-- 3.4 - Scope

-- 1
-- Yes

-- 2
-- No

-- 3
-- area d = pi * (r * r)
-- r = d / 2
-- No, the 2nd d is not in scope

-- 4
area' d = pi * (r * r)
  where
    r = d / 2
-- Yes

-- 3.5 - Syntax Errors

-- 1
-- No
e030501 = (++) [1, 2, 3] [4, 5, 6]

-- 2
-- No
e030502 = "<3" ++ "Haskell"

-- 3
-- Yes
e030503 = concat ["<3", "Haskell"]


-- 3.8 - Chapter Exercises

-- Reading syntax

-- 1
-- a
-- Yes

-- b
-- No
rs1b = (++) [1, 2, 3] [4, 5, 6]

-- c
-- Yes

-- d
-- No
rs1d = ["hello" ++ " world"]

-- e
-- No
rs1e = "hello" !! 4

-- f
-- Yes

-- g
-- No
rs1g = take 4 "lovely"

-- h
-- Yes

-- 2
-- a
rs2a = concat [[1 * 6], [2 * 6], [3 * 6]] == [6,12,18]

-- b
rs2b = "rain" ++ drop 2 "elbow" == "rainbow"

-- c
rs2c = 10 * head [1, 2, 3] == 10

-- d
rs2d = take 3 "Julie" ++ tail "yes" == "Jules"

-- e
rs2e = concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]] == [2,3,5,6,8,9]

-- Building functions

-- 1
-- a
bf1a = "Curry is awesome" ++ "!" == "Curry is awesome!"

-- b
bf1b = drop 4 (take 5 "Curry is awesome!") == "y"

-- c
bf1c = drop 9 "Curry is awesome!" == "awesome!"

-- 2
-- a
bf2a x = x ++ "!"

-- b
bf2b x = drop 4 (take 5 x)

-- c
bf2c = drop 9

-- 3
{-
thirdLetter "cafe" -- 'f'
-}
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
    a = drop 9 s

-- 6
-- see Reverse.hs
