{-# LANGUAGE InstanceSigs #-}

module Chapter25.Scratch where

import Control.Monad

--
-- 25.1 - Composing types
--

-- "functors and applicatives are both closed under composition"
--    (composing two of them returns another one of them)
-- "this is not true of monads"
-- "a monad transformer is a type constructor that takes a monad as an argument"
-- "monad transformers are never sum or product types"

--
-- 25.2 - Common functions as types
--

{-
Identity :: * -> *
      id :: a -> a
-}
newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

{-
Compose :: (* -> *) -> (* -> *) -> * -> *
    (.) :: (b -> c) -> (a -> b) -> a -> c

Compose [Just 1,Nothing] -- Compose {getCompose = [Just 1,Nothing]}

Compose [Just (1::Int),Nothing] :: Compose []   Maybe Int
                                           -f-  --g-- -a-
-}
-- remember, `f` and `g` are type constructors, not term-level functions, and `a` is a value type
--              v v v
newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

--
-- 25.3 - Two little functors sittin' in a tree, L-I-F-T-I-N-G
--

{-
fmap (+1) (Identity 2) -- Identity {runIdentity = 3}
-}
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

{-
fmap (+1) (Compose [Just 1,Nothing]) -- Compose {getCompose = [Just 2,Nothing]}
-}
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- one less bit of structure than Compose

newtype One f a = One (f a) deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

-- one more layer of structure than Compose

newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

-- we can express arbitrarily-nested types

v :: Compose [] Maybe (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]

--
-- 25.4 - Twinplicative
-- Cheated

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure fga = Compose (pure . pure $ fga)

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose fga) = Compose $ (<*>) <$> f <*> fga

--
-- 25.5 - Twonad?
--

instance (Monad f, Monad g) => Monad (Compose f g) where
  return = pure

  (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
  (>>=) = undefined                                   -- Impossible (see text)

--
-- 25.7 - Monad transformers
--

-- "multiple things going on"
-- "like IO for effectful actions plus Reader for database connections"

-- Doing it badly: one-off types

newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

newtype MaybeList a = MaybeList { runMaybeList :: [Maybe a] }

-- but we can do better
-- "as long as we know what one of the types is"

--
-- 25.8 - IdentityT
--

-- Recall from above - additional structure *could* exist:
{-
newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)
-}

-- A transformer - additional structure *does* exist
newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

-- Again, review:
{-
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
-}

instance (Functor m) => Functor (IdentityT m) where
  fmap :: (a -> b) -> IdentityT m a -> IdentityT m b
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

-- Review:
{-
instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity a) = Identity (f a)
-}

instance (Applicative m) => Applicative (IdentityT m) where
  pure :: a -> IdentityT m a
  pure x = IdentityT (pure x)

  (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

-- Review:
{-
instance Monad Identity where
  return = pure

  (Identity a) >>= f = f a
-}

-- See text for how to write this...
instance (Monad m) => Monad (IdentityT m) where
  return :: a -> IdentityT m a
  return = pure

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
