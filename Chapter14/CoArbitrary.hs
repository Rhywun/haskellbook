{-# LANGUAGE DeriveGeneric #-}

module Chapter14.CoArbitrary where

import           GHC.Generics
import           Test.QuickCheck

data Bool'
  = True'
  | False'
  deriving (Generic)

instance CoArbitrary Bool'

{-
sample' trueGen -- [0,-1,-2,1,-3,-3,-1,-5,-4,-8,-20]
-}
trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary

-- I have no idea what the point of this is.
