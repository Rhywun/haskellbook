module Exercises where

import           Control.Applicative
import           Control.Monad
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- Write instances
--
-- 1
--
data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

-- 2
--
data PhhhbbtttEither b a
  = Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' y) = Right' y
  fmap f (Left' x)  = Left' (f x)

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  Right' y <*> _ = Right' y
  _ <*> Right' y = Right' y
  Left' f <*> Left' x = Left' (f x)

instance Monad (PhhhbbtttEither b) where
  return = pure
  Right' y >>= _ = Right' y
  Left' x >>= f = f x

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left' a, Right' b]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

-- 3
--
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- 4
-- cheat cheat cheat
--
data List a
  = Nil
  | Cons a
         (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil           = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = fmap f xs `append` (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons a Nil) >>= f = f a
  (Cons a rest) >>= f = f a `append` (rest >>= f)

-- I give up
{-
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

instance EqProp (List a) where
  (=-=) = eq
-}
--
--
testWithTrigger trigger = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

main = do
  testWithTrigger (undefined :: Nope (Int, String, Int))
  testWithTrigger (undefined :: PhhhbbtttEither String (Int, String, Int))
  testWithTrigger (undefined :: Identity (Int, String, Int))
  -- testWithTrigger (undefined :: List (Int, String, Int))

-- Write functions
--
-- 1
j :: Monad m => m (m a) -> m a
j = join

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

-- 5
-- PASS
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh = undefined
--
-- 6
-- PASS
