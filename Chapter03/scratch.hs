module Scratch where



-- cont. p. 110




-- area d = pi * (r * r)
-- r = d / 2
area d = pi * (r * r)
  where
    r = d / 2

-- Ex
exclaim :: String -> String
exclaim x = x ++ "!"

getFifth :: [a] -> [a]
getFifth x = drop 4 (take 5 x)

getLastNine :: [a] -> [a]
getLastNine = drop 9

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! x

rvrs = drop 9 s ++ take 4 (drop 5 s) ++ take 5 s
  where
    s = "Curry is awesome"
