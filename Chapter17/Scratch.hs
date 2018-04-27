module Scratch where

import           Control.Applicative
import           Data.List           (elemIndex)
import           Data.Monoid

-- Applicatives are monoidal functors - LOL.
--
{-
class Functor f => Applicative f where
  pure :: a -> f a                                      -- pure 2 :: Maybe Int
                                                        -- -> Just 2
  (<*>) :: f (a -> b) -> f a -> f b
-}
--
--
-- 17.3 - Functor vs. Applicative
--
a3 = fmap (+ 1) [1, 2, 3] -- [2, 3, 4]

b3 = pure (+ 1) <*> [1, 2, 3] -- [2, 3, 4]
  -- ^^^^
  -- Notice that `pure` embeds the (+ 1) into a context

--
-- 17.4 - Applicative functors are monoidal functors
--
{-
($)   ::                    (a -> b) ->   a ->   b
(<$>) :: Functor f     =>   (a -> b) -> f a -> f b       -- <$> = fmap
(<*>) :: Applicative f => f (a -> b) -> f a -> f b       -- <*> ~ "apply"
-}
--
a4 = [(* 2), (* 3)] <*> [4, 5] -- [8, 10, 12, 15], `f` is []

b4 = Just (* 2) <*> Just 2 -- Just 4, `f` is Maybe

-- compare:
--   fmap (*2) (Just 2)     -- Just 4
c4 = Just (* 2) <*> Nothing -- Nothing

-- Show me the monoids
-- (The first items in each tuple are mappend'ed)
--
d4 = ("Woo", (+ 1)) <*> (" Hoo!", 0) -- ("Woo Hoo!", 1), `f` is (,)

e4 = (Sum 2, (+ 1)) <*> (Sum 5, 4) -- (Sum {getSum = 7}, 5)

f4 = (Product 3, (+ 9)) <*> (Product 2, 8) -- (Product {getProduct = 6}, 17)

g4 = (All True, (+ 1)) <*> (All False, 0) -- (All {getAll = False}, 1)

-- Tuple Monoid and Applicative side by side
{-
instance (Monoid a, Monoid b) => Monoid (a,b) where
  mempty = (mempty, mempty)
  (a  , b) `mappend` (a', b') = (a `mappend` a', b `mappend` b')

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)
-}
--
--
-- 17.5 - Applicative in use
--
-- List
{-
:set -XTypeApplications
(<*>) @[] :: [a -> b] -> [a] -> [b]
pure @[] :: a -> [a]
-}
a5 = [(+ 1), (* 2)] <*> [2, 4] -- -> [3,5,4,8]

b5 = (,) <$> [1, 2] <*> [3, 4] -- .. [(1, ), (2, )] <*> [3, 4]
                                      -- -> [(1,3),(1,4),(2,3),(2,4)]

c5 = liftA2 (,) [1, 2] [3, 4] -- -> [(1,3),(1,4),(2,3),(2,4)]

d5 = (+) <$> [1, 2] <*> [3, 5] -- .. [(+1), (+2)] <*> [3, 5]
                                      -- -> [4,6,5,7]

e5 = liftA2 (+) [1, 2] [3, 5] -- -> [4,6,5,7]

f5 = max <$> [1, 2] <*> [1, 4] -- .. [(max 1), (max 2)] <*> [1, 4]
                                      -- -> [1, 4, 2, 4]

g5 = liftA2 max [1, 2] [1, 4] -- -> [1, 4, 2, 4]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- -> looks up a key in an association list
h5 = lookup 3 [(3, "hello")] -- -> Just "hello"

-- More examples:
-- see lookup.hs
--
-- Exercises: Lookups
--
-- 1
--
added :: Maybe Integer
added = (+ 3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6]) -- Just 9

-- 2
--
y2 :: Maybe Integer
y2 = lookup 3 $ zip [1, 2, 3] [4, 5, 6] -- Just 6

z2 :: Maybe Integer
z2 = lookup 2 $ zip [1, 2, 3] [4, 5, 6] -- Just 5

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y2 <*> z2 -- Just (6, 5)

-- 3
--
x3 :: Maybe Int
x3 = elemIndex 3 [1, 2, 3, 4, 5] -- Just 2

y3 :: Maybe Int
y3 = elemIndex 4 [1, 2, 3, 4, 5] -- Just 3

max' :: Int -> Int -> Int -- Why is this here?
max' = max

maxed :: Maybe Int
maxed = max <$> x3 <*> y3 -- Just 3

-- 4
--
xs = [1, 2, 3]

ys = [4, 5, 6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs ys -- Just 6

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys -- Just 5

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x4 <*> y4

-- Identity
-- see identity.hs
--
-- Exercise: Identity Instance
--
newtype Identity a =
  Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

-- Constant
-- see constant.hs
--
-- Exercise: Constant Instance
--
newtype Constant a b = Constant
  { getConstant :: a
  } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant b) = Constant b

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant {getConstant = mempty}
  (<*>) (Constant x) (Constant y) = Constant (mappend x y)

-- Maybe Applicative
-- see maybe.hs
--
--
-- Exercise: Fixer Upper
--
fu1 = const <$> Just "Hello" <*> pure "World"

fu2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

--
--
-- 17.6 - Applicative laws
--
-- Identity
--
-- pure id <*> v == v
--
law1 = (pure id <*> Just "hello") == Just "hello"

-- Composition
--
-- pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
--
law2a =
  (pure (.) <*> [(+ 1)] <*> [(* 2)] <*> [1, 2, 3]) ==
  ([(+ 1)] <*> ([(* 2)] <*> [1, 2, 3])) -- [3,5,7]

law2b =
  (pure (.) <*> Just (+ 1) <*> Just (* 2) <*> Just 1) ==
  (Just (+ 1) <*> (Just (* 2) <*> Just 1)) -- Just 3

-- Homomorphism
--
-- pure f <*> pure x == pure (f x)
--
law3 = (pure (+ 1) <*> pure 2 :: Maybe Int) == (pure ((+ 1) 2) :: Maybe Int) -- Just 3

-- Interchange
--
-- u <*> pure y = pure ($ y) <*> u
--
law4a = (Just (+ 2) <*> pure 3) == (pure ($ 3) <*> Just (+ 2)) -- Just 5

law4b = ([(+ 1), (* 2)] <*> pure 1) == (pure ($ 1) <*> [(+ 1), (* 2)]) -- [2,2]

law4c = (Just (+ 3) <*> pure 1) == (pure ($ 1) <*> Just (+ 3)) -- Just 4
--
--
-- 17.7 - You knew this was coming
-- see BadMonoid.hs
--
-- 17.8 - ZipList Monoid
-- see Apl1.hs, FWIW...
