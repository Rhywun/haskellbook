module Exercises where

-- import Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- Specialize
-- 1
{-
pure :: a -> [a]

  Test:
  > let pureList = pure :: Int -> [Int]
  > pureList 1
  [1]

(<*>) :: [a -> b] -> [a] -> [b]

  Test:
  > let applyList = (<*>) :: [Int -> Int] -> [Int] -> [Int]
  > applyList [(+1)] [2,3,4]
  [3,4,5]
-}
-- 2
{-
pure :: a -> IO a

  Test:
  > let pureIO = pure :: String -> IO String
  > pureIO "hello"
  "hello"
  > :t pureIO "hello"
  pureIO "hello" :: IO String

(<*>) :: IO (a -> b) -> IO a -> IO b

  Test: How??
  It is not letting me construct an IO f
-}
-- 3
{-
pure :: a -> (a, a)                             ??
(<*>) :: (a, a -> b) -> (a, a) -> (a, b)        ??
-}
-- 4
-- PASS
--
--
-- Write Functor and Applicative instances
--
-- 1
--
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

{-
(+) <$> Pair 1 2 <*> Pair 3 4 -- Pair 4 6
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
--
data Two a b =
  Two a
      b
  deriving (Eq, Show)

{-
fmap (+1) (Two 'a' 2) -- Two 'a' 3
-}
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

{-
pure (+1) <*> (Two "a" 2) -- Two "a" 3
pure (+1) <$> (Two "a" 2) <*> (Two "b" 3) -- Two "ab" 4
-}
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

-- 3
--
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

{-
fmap (+1) (Three (Just 1) "2" 3) -- Three (Just 1) "2" 4
-}
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

{-
pure (+1) <*> (Three "a" "b" 2) -- Three "a" "b" 3
pure (+1) <$> (Three "a" "b" 2) <*> (Three "c" "d" 3) -- Three "ac" "bd" 4
pure (+1) <$> (Three (Product 3) "b" 2) <*> (Three (Product 4) "d" 3)
  -- Three (Product {getProduct = 12}) "bd" 4
-}
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three x y f) <*> (Three x' y' z) = Three (x <> x') (y <> y') (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 4
--
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

{-
fmap (+1) (Three' "yo" 2 3) -- Three' "yo" 3 4
-}
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

{-
pure (+1) <*> (Three' "a" 2 3) -- Three' "a" 3 4
pure (+1) <$> (Three' "a" 2 3) <*> (Three' "b" 4 5) -- Three' "ab" 5 6
pure (+1) <$> (Three' (Product 2) 3 4) <*> (Three' (Product 5) 6 7)
  -- Three' (Product {getProduct = 10}) 7 8
-}
instance Monoid a => Applicative (Three' a) where
  pure c = Three' mempty c c
  (Three' x f g) <*> (Three' x' y z) = Three' (x <> x') (f y) (g z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- 5
--
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

{-
fmap (+1) (Four (Just 1) "2" 3 4) -- Four (Just 1) "2" 3 5
-}
instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

{-
pure (+1) <*> (Four "a" "b" "c" 2) -- Four "a" "b" "c" 3
pure (+1) <$> (Four "a" "b" "c" 2) <*> (Four "d" "e" "f" 3)
  -- Four "ad" "be" "cf" 4
pure (+1) <$> (Four (Sum 2) (Product 3) "b" 2) <*> (Four (Sum 3) (Product 4) "d" 3)
  -- Four (Sum {getSum = 5}) (Product {getProduct = 12}) "bd" 4
-}
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four w x y f) <*> (Four w' x' y' z) =
    Four (w <> w') (x <> x') (y <> y') (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- 6
--
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' w x y f) <*> (Four' w' x' y' z) =
    Four' (w <> w') (x <> x') (y <> y') (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Four' a a a b

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- Main
--
main :: IO ()
main = do
  let trigger = ("a", "a", "a")
  quickBatch $ applicative (Pair trigger trigger)
  quickBatch $ applicative (Two trigger trigger)
  quickBatch $ applicative (Three trigger trigger trigger)
  quickBatch $ applicative (Three' trigger trigger trigger)
  quickBatch $ applicative (Four trigger trigger trigger trigger)
  quickBatch $ applicative (Four' trigger trigger trigger trigger)
--
--
-- Combinations
-- see combinations.hs
