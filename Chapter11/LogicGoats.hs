{-# LANGUAGE FlexibleInstances #-}

module Chapter11.LogicGoats where

class TooMany a where
    tooMany :: a -> Bool

instance TooMany (Int, String) where
    tooMany (n, s) = n > 42

instance TooMany (Int, Int) where
    tooMany (n1, n2) = n1 + n2 > 42

instance (Num a, TooMany a) => TooMany (a, a) where
    -- tooMany (n1, n2) = n1 + n2 > 42
    tooMany (n1, n2) = tooMany $ n1 + n2
