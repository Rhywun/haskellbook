module TypeInference where

f :: Num a => a -> a -> a
f x y = x + y + 3

f' x y = x + y + 3      -- same, Num a => a -> a -> a
