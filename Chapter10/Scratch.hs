module Chapter10.Scratch where

e01 = map (+1) [1, 2, 3]

e02 = foldr (+) 0 [1, 2, 3]

-- Notice we don't hit bottom - even though we `take 4` on a list with only 2 items
fr1 = length $ take 2 $ take 4 xs where xs = [1, 2] ++ undefined

-- No bottom here either
fr2 = foldr (\_ _ -> 9001) 0 xs where xs = [undefined, undefined]

e03 = foldr (^) 2 [1..3]

e04 = foldl (^) 2 [1..3]

-- 10.6 - How to write fold functions

hwff1 = foldr (\a b -> take 3 a ++ b) "" ["Pizza", "Apple", "Banana"]

-- 10.9 - Scans

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fib :: Int -> Integer
fib = (!!) fibs
