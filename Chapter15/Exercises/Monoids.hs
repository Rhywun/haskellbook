module Chapter15.Exercises.Monoids where

import Data.Monoid
import Test.QuickCheck

-- Shortcuts
qc prop = quickCheck prop
vc prop = verboseCheck prop

-- Associativity property
propAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
propAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Identity Left property
propIdentityL :: (Eq m, Monoid m) => m -> Bool
propIdentityL x = mempty <> x == x

-- Identity Right property
propIdentityR :: (Eq m, Monoid m) => m -> Bool
propIdentityR x = x <> mempty == x

-- 1

data Trivial = Trivial deriving (Eq, Show)

instance Monoid Trivial where
  mempty      = Trivial
  mappend _ _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2

newtype Identity a = Identity a deriving (Eq, Show)

instance Monoid a => Monoid (Identity a) where
  mempty                            = Identity mempty
  mappend (Identity x) (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3

data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two x y) (Two x' y') = Two (mappend x x') (mappend y y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 4

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj a) (BoolConj b) = BoolConj (a && b)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return (BoolConj a)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 5

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj a) (BoolDisj b) = BoolDisj (a || b)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return (BoolDisj a)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 6
-- PASS

-- 7
-- PASS


--

main :: IO ()
main = do
  qc (propAssoc :: TrivialAssoc)
  qc (propIdentityL :: Trivial -> Bool)
  qc (propIdentityR :: Trivial -> Bool)
  qc (propAssoc :: IdentityAssoc)
  qc (propIdentityL :: Identity String -> Bool)
  qc (propIdentityR :: Identity String -> Bool)
  qc (propAssoc :: TwoAssoc)
  qc (propIdentityL :: Two String String -> Bool)
  qc (propIdentityR :: Two String String -> Bool)
  qc (propAssoc :: BoolConjAssoc)
  qc (propIdentityL :: BoolConj -> Bool)
  qc (propIdentityR :: BoolConj -> Bool)
  qc (propAssoc :: BoolDisjAssoc)
  qc (propIdentityL :: BoolDisj -> Bool)
  qc (propIdentityR :: BoolDisj -> Bool)
