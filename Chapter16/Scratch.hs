{-# LANGUAGE RankNTypes #-}

module Scratch where

{-
class Functor f where                   -- `f` means "functor", which could be a list []
                                        -- or any number of other instances
  fmap :: (a -> b) -> f a -> f b
--has kind:   *    ->  *  ->  *
-}
--
--
-- 16.4 - Let’s talk about 𝑓, baby
--
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
--
-- Exercises: Be Kind
--
{-
1. a -> a
   The kind of a is *.

2. a -> b a -> T (b a)
   The kind of b is * -> *.
   The kind of T is * -> *.

3. c a b -> c b a
   The kind of c is * -> * -> *.
-}
--
--
-- 16.5 - Functor Laws
--
{-
Identity
  fmap id == id

Composition
  fmap (f . g) = fmap f . fmap g
-}
--
law1 = fmap id (Just 1) == Just 1

law2 = fmap ((+ 1) . (* 2)) [1 .. 5] == (fmap (+ 1) . fmap (* 2) $ [1 .. 5])

--
-- 16.6 - The Good, the Bad, and the Ugly
--
data WhoCares a
  = ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

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
--
data CountingBad a =
  Heisenberg Int
             a
  deriving (Eq, Show)

-- Super NOT okay! Breaks the composition law:
{-
instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg (n + 1) (f a)
-}
-- How to fix? Don't mess with type arguments that can be thought of as part of
-- the structure (i.e. which are not the final argument, in this case the `n`):
instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg n (f a)
    --         i.e.  ^                 ^            <-- Don't touch these

--
-- 16.7 - Commonly used functors
--
-- see ReplaceExperiment.hs
--
-- Exercises: Heavy Lifting
--
-- 1
a = fmap (+ 1) $ read "[1]" :: [Int]

-- 2
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3
c = fmap (* 2) (\x -> x - 2)

-- 4
d = fmap ((return '1' ++) . show) (\x -> [x,1 .. 3])

-- 5
e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = read <$> ("123" ++) <$> show <$> ioi -- Cheated here, otherwise lolwut
  in fmap (* 3) changed

--
-- 16.8 - Transforming the unapplied type argument
--
data Two a b =
  Two a
      b
  deriving (Eq, Show)

--            We can't touch this
--                    v
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

exTwo = (+ 1) <$> Two 1 2 -- Two 1 3

data Or a b
  = First a
  | Second b
  deriving (Eq, Show)

--        We can't touch this either
--                   v
instance Functor (Or a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

exOr1 = (+ 1) <$> First 3 -- First 3

exOr2 = (+ 1) <$> Second 4 -- Second 5

--
-- 16.9 - QuickChecking Functor instances
-- 16.10 - Exercises: Instances of Func
-- see QuickChecking.hs
--
--
-- 16.11 - Ignoring possibilities
--
-- Maybe: Meh
--
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing  = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing  = Nothing

-- Better (Functor pattern abstracted out)
--
incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+ 1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s

-- A little better (eta-reduced)
--
incMaybe' :: Num a => Maybe a -> Maybe a
incMaybe' = fmap (+ 1)

showMaybe' :: Show a => Maybe a -> Maybe String
showMaybe' = fmap show

-- Even better (generic)
--
liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+ 1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

-- Exercise: Possibly
--
data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

-- Either: Meh
--
incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e)  = Left e

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e)  = Left e

-- Better
--
incEither :: Num a => Either e a -> Either e a
incEither m = fmap (+ 1) m

showEither :: Show a => Either e a -> Either e String
showEither s = fmap show s

-- A little better
--
incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+ 1)

showEither' :: Show a => Either e a -> Either e String
showEither' = fmap show

-- Even better: same as `liftedInc` and `liftedShow` above!
--
--
-- Short Exercise
--
-- 1
--
data Sum a b
  = First' a
  | Second' b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First' a)  = First' a
  fmap f (Second' b) = Second' (f b)

-- 2
-- Becase the kind of Sum is * -> * -> * which isn't compatible with Functor, so
-- we partially apply the type to make it kind * -> *. The `a` associated with First'
-- is therefore applied to give (Sum a) and cannot be modified by `fmap`.
--
--
-- 16.13 - More structure, more functors
--
newtype Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa) -- `fa` means "`a` is a Functor"

--
-- 16.14 - IO Functor
--
getInt :: IO Int
getInt = fmap read getLine

meTooIsm :: IO String
meTooIsm = do
  input <- getLine
  return (input ++ " and me too!")

bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)

--
-- 16.15 - What if we want to do something different?
-- (N.B. I don't understand this. At all.)
--
-- nat :: (f -> g) -> f a -> g a
type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing  = []
maybeToList (Just a) = [a]
{-
degenerateMtl :: Nat Maybe []
degenerateMtl Nothing  = []
degenerateMtl (Just a) = [a + 1]
-}
--
--
-- 16.16 - Functors are unique to a datatype
--
-- lolwut
