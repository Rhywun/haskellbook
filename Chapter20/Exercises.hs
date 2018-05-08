module Exercises where

import           Data.Foldable
import           Data.Monoid

-- 1
--
newtype Constant a b =
  Constant b
  deriving (Eq, Show)

{-
foldMap (* 5) (Constant 100) :: Sum Int == Sum {getSum = 500}
foldr (*) 5 (Constant 6)                == 30
-}
instance Foldable (Constant a) where
  foldMap f (Constant x) = f x

-- 2
--
data Two a b =
  Two a
      b
  deriving (Eq, Show)

{-
foldMap (* 5) (Two 1 2) :: Sum Int == Sum {getSum = 10}
foldr (*) 5 (Two 4 3)              == 15
-}
instance Foldable (Two a) where
  foldMap f (Two _ y) = f y

-- 3
--
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

{-
foldr (+) 2 (Three 1 2 3) == 5
-}
instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

-- 4
--
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

{-
foldr (+) 5 (Three' 1 2 3) == 10
-}
instance Foldable (Three' a) where
  foldMap f (Three' _ y z) = f y <> f z

-- 5
--
data Four a b =
  Four a
       b
       b
       b
  deriving (Eq, Show)

{-
foldr (*) 2 (Four 3 4 5 6) == 240 -- i.e. 2 * (4 * 5 * 6)
-}
instance Foldable (Four a) where
  foldMap f (Four _ x y z) = f x <> f y <> f z

--
-- PASS
filterF ::
     (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF = undefined
