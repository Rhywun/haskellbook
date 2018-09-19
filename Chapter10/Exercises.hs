module Chapter10.Exercises where

--
-- Warm-up and review
--

-- 1

stops = "pbtdkg"

vowels = "aeiou"

-- a
threeTuples :: [(Char, Char, Char)]
threeTuples = [ (x, y, z) | x <- stops, y <- vowels, z <- stops ]

threeStrings :: [String]
threeStrings = [ [x, y, z] | x <- stops, y <- vowels, z <- stops ]

-- b
threeTuples' :: [(Char, Char, Char)]
threeTuples' = [ ('p', y, z) | y <- vowels, z <- stops ]

threeStrings' :: [String]
threeStrings' = [ ['p', y, z] | y <- vowels, z <- stops ]

-- c

nouns = ["cat", "dog", "human"]

verbs = ["eats", "loves", "pets"]

threeTuples'' :: [(String, String, String)]
threeTuples'' = [ (x, y, z) | x <- nouns, y <- verbs, z <- nouns ]

threeStrings'' :: [String]
threeStrings'' = [ x ++ " " ++ y ++ " " ++ z | x <- nouns, y <- verbs, z <- nouns ]

-- 2

{-
seekritFunc "now is the time"      -- 3
seekritFunc "resistance is futile" -- 6
seekritFunc "now is the time for all good me to come to the aid of their party" -- 3
-}
seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))
  -- Give the average length of a word in a string

-- 3

{-
seekritFunc' "now is the time"      -- 3.0
seekritFunc' "resistance is futile" -- 6.0
seekritFunc' "now is the time for all good me to come to the aid of their party" -- 3.125
-}
seekritFunc' :: String -> Double
seekritFunc' x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

--
-- Rewriting functions using folds
--

{-
and' [True, False, True] -- False
-}
and' :: [Bool] -> Bool
and' = foldr (&&) True

{-
or' [True, False, True] -- True
-}
or' :: [Bool] -> Bool
or' = foldr (||) False

{-
any' even [1,3,5] -- False
any' odd [1,3,5]  -- True
-}
any' :: (a -> Bool) -> [a] -> Bool
-- any' _ [] = False
-- any' p (x:xs) = if p x then True else any'' p xs
-- any'' p (x:xs) = p x || any'' p xs
any' p = foldr ((||) . p) False -- Cheated - how does this follow?

{-
elem' 1 [1..10] -- True
elem' 1 [2..10] -- False
-}
elem' :: Eq a => a -> [a] -> Bool
-- elem' _ [] = False
-- elem' n (x : xs) = if n == x then True else elem' n xs
-- elem' n (x : xs) = n == x || elem' n xs
elem' x = foldr ((||) . (== x)) False

{-
elem'' 1 [1..10] -- True
elem'' 1 [2..10] -- False
-}
elem'' :: Eq a => a -> [a] -> Bool
elem'' x = any' (== x)

{-
reverse' "blah" -- "halb"
reverse' [1..5] -- [5,4,3,2,1]
-}
reverse' :: [a] -> [a]
-- reverse' [x     ] = [x]
-- reverse' (x : xs) = reverse' xs ++ [x]
reverse' = foldl (flip (:)) []

{-
map' (+1) [1,2,3] -- [2,3,4]
-}
map' :: (a -> b) -> [a] -> [b]
-- map' _ []       = []
-- map' f (x : xs) = f x : map' f xs
map' f = foldr ((:) . f) [] -- Cheated

{-
filter' (> 2) [1,2,3,4] -- [3,4]
-}
filter' :: (a -> Bool) -> [a] -> [a]
-- filter' _ []       = []
-- filter' p (x : xs) = if p x then x : filter' p xs else filter' p xs
filter' p = foldr (\a b -> if p a then a : b else b) [] -- Cheated but I get it

{-
squish [[1, 2], [3, 4]] -- [1, 2, 3, 4]
-}
squish :: [[a]] -> [a]
-- squish [x] = [x] -- <-- Error, no idea how to write this
-- squish (x:xs) = x ++ squish xs
squish = foldr (++) [] -- Cheated

{-
squishMap (\x -> [1, x, 3]) [2]                 -- [1,2,3]
squishMap (\x -> "WO " ++ [x] ++ " OT ") "blah" -- "WO b OT WO l OT WO a OT WO h OT "
-}
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

{-
squishAgain [[1, 2], [3, 4]] -- [1, 2, 3, 4]
-}
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy = undefined
-- PASS