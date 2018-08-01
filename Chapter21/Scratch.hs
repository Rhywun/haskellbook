module Scratch where

import           Data.Functor.Constant
import           Data.Functor.Identity
import           Data.Monoid

--
{-
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f

  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id
-}
--
--
-- 21.3 - sequenceA
--
-- It flips the layers of structure around:
{-
sequenceA [Just 1,Just 2,Just 3] == Just [1,2,3]
-}
--
--
-- 21.4 - traverse
--
{-
sequenceA . fmap Just $ [1,2,3] == Just [1,2,3]
traverse Just [1,2,3]           == Just [1,2,3]
-}
--
--
-- 21.5 - So, what's Traversable for?
--
{-
let f = undefined :: a -> Maybe b
let xs = undefined :: [a]
:t map f xs                 -- [Maybe b]
:t traverse f xs            -- Maybe [b]
-}
--
--
-- 21.7 - Axing tedious code
--
--
data Query =
  Query

data SomeObj =
  SomeObj

data IoOnlyObj =
  IoOnlyObj

data Err =
  Err

-- There's a decoder function that makes some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- There's a query, that runs against the DB and returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- an additional "context initializer", that also has IO
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- BEFORE
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  -- case sequence (map decodeFn a) of
  case mapM decodeFn a of
    (Left err) -> return $ Left err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

-- AFTER
pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (mapM decodeFn a)

--
--
-- 21.8 - Do all the things
-- see HttpStuff.hs
--
su1 = traverse (Identity . (+ 1)) [1, 2] -- Identity [2,3]

su2 = runIdentity $ traverse (Identity . (+ 1)) [1, 2] -- [2,3]

edgeMap :: Traversable t => (a -> b) -> t a -> t b
edgeMap f t = runIdentity $ traverse (Identity . f) t

{-
compare:
map :: (a -> b) -> [a] -> [b]
-}
--
su3 = edgeMap (+ 1) [1 .. 5] -- [2,3,4,5,6]

su4 = traverse (Constant . (+ 1)) ([1, 2, 3, 4, 5] :: [Sum Integer])
  -- Constant (Sum {getSum = 20})

foldMap' :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f t = getConstant $ traverse (Constant . f) t

{-
compare:
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-}
--
--
-- 21.9 - Traversable instances
--
-- Either
--
data Either' a b
  = Left' a
  | Right' b
  deriving (Eq, Ord, Show)

instance Functor (Either' a) where
  fmap _ (Left' x)  = Left' x
  fmap f (Right' y) = Right' (f y)

instance Applicative (Either' a) where
  pure = Right'
  Left' x <*> _ = Left' x
  Right' f <*> r = fmap f r

instance Foldable (Either' a) where
  foldMap _ (Left' _)  = mempty
  foldMap f (Right' y) = f y
  foldr _ z (Left' _)  = z
  foldr f z (Right' y) = f y z

instance Traversable (Either' a) where
  traverse _ (Left' x)  = pure (Left' x)
  traverse f (Right' y) = Right' <$> f y

--
-- Tuple
--
data Two a b =
  Two a
      b
  deriving (Eq, Ord, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two u f) <*> (Two v x) = Two (u `mappend` v) (f x)

instance Foldable (Two a) where
  foldMap f (Two _ y) = f y
  foldr f z (Two _ y) = f y z

instance Traversable (Two a) where
  traverse f (Two x y) = Two x <$> f y
--
--
-- 21.10 - Traversable Laws
--
{-
Naturality:
  t . traverse f = traverse (t . f)

Identity:
  traverse Identity = Identity

Composition:
  traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
-}
