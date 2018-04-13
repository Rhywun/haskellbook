{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Generalize where

class TooMany a
    where tooMany :: a -> Bool

instance TooMany Int
    where tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

{-
Now we don't need this:
instance TooMany Goats where
    tooMany (Goats n) = tooMany n
-}
