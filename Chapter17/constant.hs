import Data.Functor.Constant
import Data.Monoid

type C = Constant

f = Constant (Sum 1)
g = Constant (Sum 2)

r = f <*> g

