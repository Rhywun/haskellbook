module Exercises where

import Data.Time

-- 10.6 - Database Processing

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = [date | (DbDate date) <- db]

-- Again with the cheating. I'm guessing the author wants us to use a fold here but I don't
-- see where to begin.

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = [number | (DbNumber number) <- db]

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral $ sum aFilter) / (fromIntegral $ length aFilter)
    where aFilter = filterDbNumber db

-- 10.9 - Scans Exercises

fibs :: [Integer]
fibs = take 20 $ 1 : scanl (+) 1 fibs

fibs' :: [Integer]
fibs' = filter (<100) (1 : scanl (+) 1 fibs)

factorials = take 5 $ scanl (*) 1 [2..]

-- 10.10 - Warm-up and review

stops = "pbtdkg"
vowels = "aeiou"

threeTuples :: [(Char, Char, Char)]
threeTuples = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

threeTuples' :: [(Char, Char, Char)]
threeTuples' = [('p', y, z) | y <- vowels, z <- stops]

nouns = ["cat", "dog", "human"]
verbs = ["eats", "loves", "pets"]

threeTuples'' :: [(String, String, String)]
threeTuples'' = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

seekritFunc :: String -> Int
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))
-- Give the average length of a word in a string

seekritFunc' :: String -> Double
seekritFunc' x =
    (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

-- 10.10 - Rewriting functions using folds

or' :: [Bool] -> Bool
or' = foldr (||) False

any' :: (a -> Bool) -> [a] -> Bool
any' = undefined

--
-- I'm giving up before I quit the book entirely.
--
