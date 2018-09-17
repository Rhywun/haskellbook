module Chapter10.Exercises where

--
-- Warm-up and review
--
-- 1
--
stops = "pbtdkg"

vowels = "aeiou"

-- a
threeTuples :: [(Char, Char, Char)]
threeTuples = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

threeStrings = [[x, y, z] | x <- stops, y <- vowels, z <- stops]

-- b
threeTuples' :: [(Char, Char, Char)]
threeTuples' = [('p', y, z) | y <- vowels, z <- stops]

threeStrings' = [['p', y, z] | y <- vowels, z <- stops]

-- c
--
nouns = ["cat", "dog", "human"]

verbs = ["eats", "loves", "pets"]

threeTuples'' :: [(String, String, String)]
threeTuples'' = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

-- 2
{-
seekritFunc "now is the time" == 3
seekritFunc "resistance is futile" == 6
-}
seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))
  -- Give the average length of a word in a string

seekritFunc' :: String -> Double
seekritFunc' x =
  (fromIntegral (sum (map length (words x)))) /
  (fromIntegral (length (words x)))

--
-- Rewriting functions using folds
--
or' :: [Bool] -> Bool
or' = foldr (||) False

any' :: (a -> Bool) -> [a] -> Bool
any' = undefined

-- Totally lost here.
