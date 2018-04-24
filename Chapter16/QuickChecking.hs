module QuickChecking where

import           Test.QuickCheck

--
-- 16.9 - QuickChecking Functor instances
--
-- Properties for our laws:
--
propFunctorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
propFunctorIdentity f = fmap id f == f

propFunctorCompose ::
     (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
propFunctorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

main1 :: IO ()
main1 = do
  quickCheck (propFunctorIdentity :: [Int] -> Bool)
  let c = propFunctorCompose (+ 1) (* 2)
  let li x = c (x :: [Int])
  quickCheck li

-- Omitting CoArbitrary stuff because brain hurts
--
--
-- 16.10 - Exercises: Instances of Func
--
-- 1
--
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

-- 2
--
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

-- 3
--
data Two a b =
  Two a
      b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

-- 4
--
data Three a b c =
  Three a
        b
        c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

-- 5
--
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

-- 6
--
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

-- 7
--
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Four' a a a b)

-- 8
--
data Trivial =
  Trivial --
    -- Can't implement Functor because there isn't a type argument (kind is *).

--
main2 :: IO ()
main2 = do
  quickCheck (propFunctorIdentity :: Identity Int -> Bool)
  quickCheck (propFunctorIdentity :: Pair Char -> Bool)
  quickCheck (propFunctorIdentity :: Two Int Char -> Bool)
  quickCheck (propFunctorIdentity :: Three String Int Char -> Bool)
  quickCheck (propFunctorIdentity :: Three' Int Char -> Bool)
  quickCheck (propFunctorIdentity :: Four Int Int Int Int -> Bool)
  quickCheck (propFunctorIdentity :: Four' String Int -> Bool)

  quickCheck (\x -> propFunctorCompose (+ 1) (* 2) (x :: Identity Int))
  quickCheck (\x -> propFunctorCompose (+ 1) (* 2) (x :: Pair Int))
  quickCheck (\x -> propFunctorCompose (+ 1) (* 2) (x :: Two Int Int))
  quickCheck (\x -> propFunctorCompose (+ 1) (* 2) (x :: Three Int Int Int))
  quickCheck (\x -> propFunctorCompose (+ 1) (* 2) (x :: Three' Int Int))
  quickCheck (\x -> propFunctorCompose (+ 1) (* 2) (x :: Four Int Int Int Int))
  quickCheck (\x -> propFunctorCompose (+ 1) (* 2) (x :: Four' Int Int))
