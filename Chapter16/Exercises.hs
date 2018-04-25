{-# LANGUAGE FlexibleInstances #-}

module Chapter16.Exercises where

import           GHC.Arr

-- Has valid functor?
--
-- 1
-- No, because its kind is *.
data Bool'
  = False'
  | True'

-- 2
-- Yes, because its kind is * -> *.
data BoolAndSomethingElse a
  = False'' a
  | True'' a

-- 3
-- Yes, because its kind is * -> *.
data BoolAndMaybeSomethingElse a
  = Falsish
  | Truish a

-- 4
-- Yes, because its kind is (* -> *) -> *.
newtype Mu f = InF
  { outF :: f (Mu f)
  }

-- 5
-- No, because its kind is *.
data D =
  D (Array Word Word)
    Int
    Int

-- Rearrange type constructor arguments
--
-- 1
--
data Sum a b
  = First b
  | Second a
  deriving (Show)

{-
(+1) <$> (First 2) == First 3
-}
instance Functor (Sum e) where
  fmap f (First a)  = First (f a)
  fmap f (Second b) = Second b

-- 2
--
data Company a b c
  = DeepBlue a
             b
  | Something c
  deriving (Show)

{-
(+1) <$> (Something 3) == Something 4
-}
instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3
--
data More a b
  = L b
      a
      b
  | R a
      b
      a
  deriving (Eq, Show)

{-
(+1) <$> (L 1 2 3) == L 2 2 4
(+1) <$> (R 1 2 3) == R 1 3 3
-}
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write functor instances
--
-- 1
--
data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2
--
newtype K a b =
  K a

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3
--
newtype Flip f a b =
  Flip (f b a)

newtype K' a b =
  K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip $ K' (f a)

-- 4
--
newtype EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
--
newtype LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut a) = LiftItOut (fmap f a)

-- 6
--
data Parappa f g a =
  DaWrappa (f a)
           (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa a a') = DaWrappa (fmap f a) (fmap f a')

-- 7
-- PASS
data IgnoreOne f g a b =
  IgnoringSomething (f a)
                    (g b) --
--
-- 8, 9, 10, 11
-- PASS, PASS, PASS, PASS
