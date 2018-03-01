module Chapter17.Exercises where

-- import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Specialize

-- 1
{-
pure :: a -> [a]
(<*>) :: [a -> b] -> [a] -> [b]
-}

-- 2
{-
pure :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b
-}

-- 3
{-
pure :: a -> (a, a)
(<*>) :: (a, a -> b) -> (a, a) -> (a, b)
-}

-- 4
-- PASS

-- Instances

-- 1

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

{-
E.g. (+) <$> Pair 1 2 <*> Pair 3 4        -- Pair 4 6
-}
instance Applicative Pair where
  pure x = Pair x x
  Pair f g <*> Pair x y = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- 2
-- Looks like a tuple

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two a f) (Two a' x) = Two (a <> a') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

--

main :: IO ()
main = do
  let x = ("a", "a", "a")
  quickBatch $ applicative (Pair x x)
  quickBatch $ applicative (Two x x)
