module Semigroups where

import           Data.Semigroup
import           Test.QuickCheck

-- Shortcuts
qc prop = quickCheck prop

vc prop = verboseCheck prop

-- Associativity property
propAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
propAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- 1
--
data Trivial =
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
--
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc
   = Identity String -> Identity String -> Identity String -> Bool

-- 3
--
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x y <> Two x' y' = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc
   = Two String String -> Two String String -> Two String String -> Bool

-- 4
--
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  Three x y z <> Three x' y' z' = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc
   = Three (Sum Int) String String -> Three (Sum Int) String String -> Three (Sum Int) String String -> Bool

-- 5
--
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  Four w x y z <> Four w' x' y' z' =
    Four (w <> w') (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc
   = Four (Sum Int) (Product Int) String String -> Four (Sum Int) (Product Int) String String -> Four (Sum Int) (Product Int) String String -> Bool

-- 6
--
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj a <> BoolConj b = BoolConj (a && b)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return (BoolConj a)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7
--
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj a <> BoolDisj b = BoolDisj (a || b)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return (BoolDisj a)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8
--
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst _ <> r = r
  l <> _ = l

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return (Fst a), return (Snd b)]

type OrAssoc = Or Char Int -> Or Char Int -> Or Char Int -> Bool

-- 9
--
newtype Combine a b = Combine
  { unCombine :: a -> b
  }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)
    -- Cheated, and I still don't understand it. How do you generate
    -- arbitrary functions in order to test it with QuickCheck?!
    -- ...
    -- Ah yes, `CoArbitrary` which was poorly described earlier...

--
-- 10
-- Cheat, cheat, cheat...
newtype Comp a = Comp
  { unComp :: a -> a
  }

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

genFunc :: (CoArbitrary a, Arbitrary a) => Gen (a -> a)
genFunc = arbitrary

genComp :: (CoArbitrary a, Arbitrary a) => Gen (Comp a)
genComp = do
  f <- genFunc
  return Comp {unComp = f}

-- Begin not cheat...
--
instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = genComp

type CompAssoc = Comp Int -> Comp Int -> Comp Int -> Bool

--
--
main :: IO ()
main = do
  qc (propAssoc :: TrivialAssoc)
  qc (propAssoc :: IdentityAssoc)
  qc (propAssoc :: TwoAssoc)
  qc (propAssoc :: ThreeAssoc)
  qc (propAssoc :: FourAssoc)
  qc (propAssoc :: BoolConjAssoc)
  qc (propAssoc :: BoolDisjAssoc)
  qc (propAssoc :: OrAssoc)
  -- qc (propAssoc :: CompAssoc)        No idea how to get this to work.

-- 11
-- See Validation.hs
