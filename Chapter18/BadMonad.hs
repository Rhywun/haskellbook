module BadMonad where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data CountMe a =
  CountMe Integer
          a
  deriving (Eq, Show)

-- Bad:
-- instance Functor CountMe where
--   fmap f (CountMe i a) = CountMe (i + 1) (f a)
--
-- Fixed:
instance Functor CountMe where
  fmap f (CountMe i a) = CountMe i (f a)

-- Bad:
-- instance Applicative CountMe where
--   pure = CountMe 0
--   CountMe n f <*> CountMe _ a = CountMe (n + 1) (f a)
--
-- Fixed:
instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

-- Bad:
-- instance Monad CountMe where
--   return = pure
--   CountMe n a >>= f =
--     let CountMe _ b = f a
--     in CountMe (n + 1) b
--
-- Still bad:
-- instance Monad CountMe where
--   return = pure
--   CountMe _ a >>= f = f a
--
-- Fixed:
instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
