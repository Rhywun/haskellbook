module Chapter17.Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap = undefined
  
instance Applicative List where
  pure = undefined
  (<*>) = undefined
