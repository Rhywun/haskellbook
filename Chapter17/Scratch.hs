module Chapter17.Scratch where

import Data.Monoid

-- Applicatives are monoidal functors, LOL.

{-
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

-- 17.3 - Functor vs. Applicative

a = fmap (+1) [1,2,3]         -- [2, 3, 4]
b = pure (+1) <*> [1,2,3]     -- [2, 3, 4]

-- 17.4 - Applicative functors are monoidal functors

{-
($)   ::                    (a -> b) ->   a ->   b
(<$>) :: Functor f     =>   (a -> b) -> f a -> f b       -- <$> = fmap
(<*>) :: Applicative f => f (a -> b) -> f a -> f b       -- <*> ~ "apply"
-}

c = [(*2), (*3)] <*> [4, 5]   -- [8, 10, 12, 15], `f` is []
d = Just (*2) <*> Just 2      -- Just 4, `f` is Maybe
e = Just (*2) <*> Nothing     -- Nothing

-- Show me the monoids

-- The first items in each tuple are mappend'ed:
f = ("Woo", (+1)) <*> (" Hoo!", 0)          -- ("Woo Hoo!", 1), `f` is (,)
g = (Sum 2, (+1)) <*> (Sum 0, 0)            -- (Sum {getSum = 2}, 1)
h = (Product 3, (+9)) <*> (Product 2, 8)    -- (Product {getProduct = 6}, 17)
i = (All True, (+1)) <*> (All False, 0)     -- (All {getAll = False}, 1)



-- cont. p. 1066
£