module Chapter08.Scratch where

-- 8.2 - Factorial!

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n - 1)

--

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n - 1) f b)

{-
applyTimes 5 (+1) 5
(+1) (applyTimes 4 (+1) 5)
(+1) ((+1) (applyTimes 3 (+1) 5))
(+1) ((+1) ((+1) (applyTimes 2 (+1) 5)))
(+1) ((+1) ((+1) ((+1) (applyTimes 1 (+1) 5))))
(+1) ((+1) ((+1) ((+1) ((+1) (applyTimes 0 (+1) 5)))))
(+1) ((+1) ((+1) ((+1) ((+1) 5))))
(+1) ((+1) ((+1) ((+1) 6)))
(+1) ((+1) ((+1) 7))
(+1) ((+1) 8)
(+1) 9
10
-}

-- 8.3 - Bottom

f :: Bool -> Int
f True = error "blah"
f False = 0

-- partial:
f' :: Bool -> Int
f' False = 0

-- total:
f'' :: Bool -> Maybe Int
f'' False = Just 0
f'' _     = Nothing

-- 8.4 - Fibonacci numbers

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

-- 8.5 - Integral division from scratch

-- A "go function" defined in the where clause allows us to track the count of recursions (in `i`)
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy n d = go n d 0
    where go n' d' i | n' < d'   = (i, n')
                     | otherwise = go (n' - d') d' (i + 1)
