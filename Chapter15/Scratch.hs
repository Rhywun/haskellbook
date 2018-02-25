module Chapter15.Scratch where

import Data.Monoid
import Test.QuickCheck

-- 15.4 How Monoid is defined in Haskell

{-
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
-}

-- 15.8 - Laws

{-
-- left identity
mappend mempty x = x

-- right identity
mappend x mempty = x

-- associativity
mappend x (mappend y z) = mappend (mappend x y) z
-- or
x <> (y <> z) = (x <> y) <> z

mconcat = foldr mappend mempty
-}

-- 15.10 - Reusing algebras by asking for algebras
-- (Another `Monoid` for `Maybe`)

-- We don't need a `Monoid` instance for `a`:

data Booly a = False' | True' deriving (Eq, Show)

instance Monoid (Booly a) where
  mempty              = False'    -- -Wmissing-methods without this
  mappend False' _    = False'
  mappend _ False'    = False'
  mappend True' True' = True'

-- Exercise: Optional Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty                    = Nada
  mappend Nada m            = m
  mappend m Nada            = m
  mappend (Only m) (Only n) = Only (mappend m n)

-- Associativity
-- (vs Commutativity)

evilPlus = flip (+)
evilPlusPlus = flip (++)

-- 15.11 - Madness

type V = String
type Adj = String
type Adv = String
type N = String
type Excl = String

madlibbin' :: Excl -> Adv -> N -> Adj -> String
madlibbin' excl adv n adj =
  mconcat [excl, "! he said ", adv, " as he jumped into his car ",
           n, " and drove off with his ", adj, " wife."]

-- 15.12 - Better living through QuickCheck

-- Shortcuts
qc prop = quickCheck prop
vc prop = verboseCheck prop

-- A statement of associativity
asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c = a <> (b <> c) == (a <> b) <> c

-- Here's a property to test the statement
prop_monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
prop_monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
-- quickCheck (prop_monoidAssoc :: String -> String -> String -> Bool)

prop_monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidLeftIdentity a = (mempty <> a) == a

prop_monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidRightIdentity a = (a <> mempty) == a

-- Testing QuickCheck's patience
-- (demonstrate why a Bool Monoid can't have False as the identity)

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools) , (1, return Twoo)]

instance Monoid Bull where
  mempty      = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

qcBull :: IO ()
qcBull = do
  let ma  = prop_monoidAssoc
      mli = prop_monoidLeftIdentity
      mlr = prop_monoidRightIdentity
  qc (ma :: BullMappend)
  qc (mli :: Bull -> Bool)
  qc (mlr :: Bull -> Bool)

-- Exercise: Maybe Another Monoid

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (2, genOnly)]
    where genOnly = do
            a <- arbitrary
            return (Only a)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    return First' { getFirst' = a }

instance Monoid (First' a) where
  mempty                  = First' Nada
  mappend (First' Nada) r = r
  mappend l _             = l

qcFirst' :: IO ()
qcFirst' = do
  quickCheck (prop_monoidAssoc :: First' String -> First' String -> First' String -> Bool)
  quickCheck (prop_monoidLeftIdentity :: First' String -> Bool)
  quickCheck (prop_monoidRightIdentity :: First' String -> Bool)
