{-# LANGUAGE DeriveGeneric #-}

module CoArbitrary where

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
trueGen = coarbitrary True' arbitrary :: Gen Int

falseGen = coarbitrary False' arbitrary :: Gen Int

-- I have no idea what the point of this is.
