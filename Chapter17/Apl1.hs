module Apl1 where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

{-
instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary
-}
instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

-- List Applicative Exercise
--    cheat cheat cheat
--
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
  fmap _ Nil           = Nil
  fmap f (Cons x rest) = Cons (f x) (fmap f rest)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _          = Nil
  (<*>) _ Nil          = Nil
  (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)

{-
> f = Cons (+1) (Cons (*2) Nil)
> v = Cons 1 (Cons 2 Nil)
> f <*> v
  Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
-}
--
instance EqProp List where
  (=-=) = eq


-- totally lost here