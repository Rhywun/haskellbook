{-# LANGUAGE InstanceSigs #-}

module Chapter25.Scratch where

--
-- 25.1 - Composing types
--

-- "functors and applicatives are both closed under composition"
--    (composing two of them returns another one of them)
-- "this is not true of monads"

--
-- 25.2 - Common functions as types
--

-- "a monad transformer is a type constructor that takes a monad as an argument"

newtype Identity a = Identity { runIdentity :: a }

-- "monad transformers are never sum or product types"

{-
Compose [Just 1,Nothing] -- Compose {getCompose = [Just 1,Nothing]}

Compose [Just (1::Int),Nothing] :: Compose [] Maybe Int
-}
newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

--
-- 25.3 - Two little functors sittin' in a tree, L-I-F-T-I-N-G
--

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

--

newtype One f a = One (f a) deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

--

newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

--

v :: Compose [] Maybe (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]

--
-- 25.4 - Twinplicative
--

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = undefined

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = undefined
