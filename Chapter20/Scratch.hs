module Scratch where

import           Data.Foldable
import           Data.Monoid

--
-- 20.4 - Demonstrating Foldable instances
--
-- Identity
--
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

-- "Maybe"
--
data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada    = z
  foldr f z (Yep x) = f x z
  foldl _ z Nada    = z
  foldl f z (Yep x) = f z x
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a
--
--
-- 20.5 - Some basic derived operations
--
-- Exercises: Library Functions
--
-- 1
sum' :: (Num a, Foldable t) => t a -> a
sum' = undefined

-- Yeah, I dunno.
