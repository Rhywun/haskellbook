module Chapter16.Scratch where

{-
class Functor f where                   -- `f` means "functor", which could be a list []
                                        -- or any number of other instances
  fmap :: (a -> b) -> f a -> f b
--has kind:   *    ->  *  ->  *
-}

-- 16.4 - Let’s talk about 𝑓, baby

{-
Prelude> :k (->)
(->) :: TYPE q -> TYPE r -> *           -- Wut?
-}

class Sumthin a where
  s :: a -> a

-- These don't compile
{-
class Else where
  e :: b -> f (g a b c)

class Biffy where
  slayer :: e a b -> (a -> c) -> (b -> d) -> e c d
-}

-- Exercises: Be Kind

{-
1. a -> a
   The kind of a is *.

2. a -> b a -> T (b a)
   The kind of b is * -> *.
   The kind of T is * -> *.

3. c a b -> c b a
   The kind of c is * -> * -> *.
-}

-- 16.5 - Functor Laws

{-
Identity
  fmap id == id

Composition
  fmap (f . g) = fmap f . fmap g
-}

-- 16.6 - The Good, the Bad, and the Ugly

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)

-- Good:
instance Functor WhoCares where
  fmap _ ItDoesnt         = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a)       = Matter (f a)

-- Bad! Breaks the identity law:
{-
instance Functor WhoCares where
  fmap _ ItDoesnt = WhatThisIsCalled
  fmap f WhatThisIsCalled = ItDoesnt
  fmap f (Matter a) = Matter (f a)
-}

data CountingBad a = Heisenberg Int a deriving (Eq, Show)

-- super NOT okay! Breaks the composition law:
{-
instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)
-}

-- How to fix? Don't mess with type arguments that are part of the structure:
instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg n (f a)
--             i.e.  ^                 ^

-- 16.7 - Commonly used functors

-- Exercises: Heavy Lifting

-- 1
a = fmap (+1) $ read "[1]" :: [Int]

-- 2
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3
c = fmap (*2) (\x -> x - 2)

-- 4
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- 5
{- e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read ("123"++) show ioi
    in (*3) changed
-}
-- PASS

-- 16.8 - Transforming the unapplied type argument

data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Or a b = First a | Second b deriving (Eq, Show)
instance Functor (Or a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

-- 16.9 - QuickChecking Functor instances

-- Properties for our laws:

prop_functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
prop_functorIdentity f = fmap id f == f

prop_functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
prop_functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- 16.10 - Exercises: Instances of Func

-- 1

{-
newtype Identity a = Identity a
instance Functor Identity where
  fmap _ (Identity a) = Identity a
-}
