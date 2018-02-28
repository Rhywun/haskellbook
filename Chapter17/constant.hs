import Data.Functor.Constant
import Data.Monoid

{-
-- f ~ Constant e
type C = Constant

(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: C e (a -> b) -> C e a -> C e b

pure :: a -> f a
pure :: a -> C e a
-}

f = Constant (Sum 1)
g = Constant (Sum 2)

r = f <*> g

