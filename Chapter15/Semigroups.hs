module Semigroups where

import           Data.List.NonEmpty
import           Data.Semigroup

xs = 1 :| [2, 3] :: NonEmpty Integer

ys = 4 :| [5, 6] :: NonEmpty Integer
--
{-
xs <> ys == 1 :| [2,3,4,5,6]
-}
--
{-
NonEmpty is a Semigroup because it has no identity,
i.e. you cannot make an empty list of it.
-}
