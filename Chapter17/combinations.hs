import Control.Applicative (liftA3)

stops = "pbtdkg"
vowels = "aeiou"

-- From Chapter 10:
threeTuples :: [(Char, Char, Char)]
threeTuples = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

-- combos :: [a] -> [b] -> [c] -> [(a, b, c)]     -- WTF?
combos :: [(Char, Char, Char)]
combos = liftA3 (,,) stops vowels stops
