module Chapter17.Scratch where

import Data.Monoid
import Control.Applicative
import Data.List (elemIndex)

-- Applicatives are monoidal functors, LOL.

{-
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

-- 17.3 - Functor vs. Applicative

a3 = fmap (+1) [1,2,3]         -- [2, 3, 4]
b3 = pure (+1) <*> [1,2,3]     -- [2, 3, 4]

-- 17.4 - Applicative functors are monoidal functors

{-
($)   ::                    (a -> b) ->   a ->   b
(<$>) :: Functor f     =>   (a -> b) -> f a -> f b       -- <$> = fmap
(<*>) :: Applicative f => f (a -> b) -> f a -> f b       -- <*> ~ "apply"
-}

a4 = [(*2), (*3)] <*> [4, 5]   -- [8, 10, 12, 15], `f` is []
b4 = Just (*2) <*> Just 2      -- Just 4, `f` is Maybe
c4 = Just (*2) <*> Nothing     -- Nothing

-- Show me the monoids

-- The first items in each tuple are mappend'ed:
d4 = ("Woo", (+1)) <*> (" Hoo!", 0)          -- ("Woo Hoo!", 1), `f` is (,)
e4 = (Sum 2, (+1)) <*> (Sum 0, 0)            -- (Sum {getSum = 2}, 1)
f4 = (Product 3, (+9)) <*> (Product 2, 8)    -- (Product {getProduct = 6}, 17)
g4 = (All True, (+1)) <*> (All False, 0)     -- (All {getAll = False}, 1)

-- Tuple Monoid and Applicative side by side

{-
instance (Monoid a, Monoid b) => Monoid (a,b) where
  mempty = (mempty, mempty)
  (a  , b) `mappend` (a',b') = (a `mappend` a', b `mappend` b')

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)
-}

-- 17.5 - Applicative in use

-- List
{-
(<*>) @[] :: [a -> b] -> [a] -> [b]
pure @[] :: a -> [a]
-}

a5 = [(+1), (*2)] <*> [2, 4]          -- -> [3,5,4,8]

b5 = (,) <$> [1, 2] <*> [3, 4]        -- .. [(1, ), (2, )] <*> [3, 4]
                                      -- -> [(1,3),(1,4),(2,3),(2,4)]
c5 = liftA2 (,) [1, 2] [3, 4]         -- -> [(1,3),(1,4),(2,3),(2,4)]

d5 = (+) <$> [1, 2] <*> [3, 5]        -- .. [(+1), (+2)] <*> [3, 5]
                                      -- -> [4,6,5,7]
e5 = liftA2 (+) [1, 2] [3, 5]         -- -> [4,6,5,7]

f5 = max <$> [1, 2] <*> [1, 4]        -- .. [(max 1), (max 2)] <*> [1, 4]
                                      -- -> [1, 4, 2, 4]
g5 = liftA2 max [1, 2] [1, 4]         -- -> [1, 4, 2, 4]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- -> looks up a key in an association list

h5 = lookup 3 [(3, "hello")]          -- -> Just "hello"

-- Exercises: Lookups

-- 1

added :: Maybe Integer
added = (+ 3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

-- 2

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' x y


-- cont. p. 1077
