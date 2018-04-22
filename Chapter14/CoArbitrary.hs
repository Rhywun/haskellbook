{-# LANGUAGE DeriveGeneric #-}

module CoArbitrary where

import           GHC.Generics
import           Test.QuickCheck

data Bool'
  = True'
  | False'
  deriving (Generic)

instance CoArbitrary Bool'

trueGen = coarbitrary True' arbitrary :: Gen Int

falseGen = coarbitrary False' arbitrary :: Gen Int

-- I have no idea what the point of this is.
