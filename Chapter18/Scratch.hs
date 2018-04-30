module Scratch where

import           Control.Applicative
import           Control.Monad
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- "Monads are applicative functors." Oh, boy.
--
-- In order of power:
-- Functor -> Applicative -> Monad
--
-- Definition:
{-
class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b    -- AKA "bind"
  (>>)   :: m a -> m b        -> m b
  return :: a   -> m a                  -- <-- Same as `pure`
-}
--
-- A law:
{-
fmap f xs = xs >>= return . f

fmap (+1) [1..3]            -- [2, 3, 4]
[1..3] >>= return . (+1)    -- [2, 3, 4]
-}
--
-- The novel part of Monad - it removes an outer layer of context:
{-
join :: Monad m => m (m a) -> m a       -- import Control.Monad (join)

compare:
concat ::           [[a]]  ->  [a]
-}
--
-- Write `bind` in terms of `fmap` and `join`:
-- keep in mind this is (>>=) flipped                 -- compare to type signature above
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f -- OK... lolcheat
  -- `join` removes structure that was added by `fmap`

-- zipWith
{-
Compare:

zipWith (+) [3, 4] [5, 6] -- [8, 10]
liftA2  (+) [3, 4] [5, 6] -- [8, 9, 9, 10]
  -- Remember, the applicative monoid over lists resembles the Cartesian product!

zipWith3 (,,) [1, 2] [3, 4] [5, 6] -- [(1,3,5),(2,4,6)]
liftM3   (,,) [1, 2] [3, 4] [5, 6]
  -- [(1,3,5),(1,3,6),(1,4,5),(1,4,6),(2,3,5),(2,3,6),(2,4,5),(2,4,6)]
-}
--
--
-- 18.3 - Do syntax and monads
--
-- Note the sequencing functions:
{-
(*>) :: Applicative f => f a -> f b -> f b
(>>) :: Monad m       => m a -> m b -> m b
-}
--
sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' = putStrLn "blah" >> putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' = putStrLn "blah" *> putStrLn "another thing"

--
binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn -- Looks like threading or piping to me...

binding'' :: IO ()
binding'' = join $ putStrLn <$> getLine

-- Interlude...
{-
Compare:
join $ putStrLn <$> getLine
getLine >>= putStrLn
-}
--
bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >> getLine >>= \name ->
    putStrLn ("y helo thar: " ++ name)

-- `do` sugar really helps when the nesting starts to get out of hand:
twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:" >> getLine >>= \name ->
    putStrLn "age pls:" >> getLine >>= \age ->
      putStrLn ("y helo thar: " ++ name ++ " who is: " ++ age ++ " years old.")

--
-- 18.4 - Examples of Monad use
--
-- List
{-
(>>=)  @[] :: [a] -> (a -> [b]) -> [b]
return @[] ::  a ->  [a]
-}
--
-- These work just like list comprehensions:
{-
twiceWhenEven [1..10] == [1,4,4,9,16,16,25,36,36,49,64,64,81,100,100]
-}
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else [x * x]

{-
twiceWhenEven' [1..10] == [1,9,25,49,81]
-}
twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then []
    else [x * x]

-- Maybe
{-
(>>=)  @Maybe :: Maybe a -> (a -> Maybe b) -> Maybe b
return @Maybe :: a -> Maybe a
-}
--
data Cow = Cow
  { name   :: String
  , age    :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
       then Nothing
       else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing      -> Nothing
            Just weighty -> weightCheck (Cow nammy agey weighty)

-- We can do better
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name age weight = do
  name' <- noEmpty name
  age' <- noNegative age
  weight' <- noNegative weight
  weightCheck (Cow name' age' weight')

-- Or
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name age weight =
  noEmpty name >>= \name' ->
    noNegative age >>= \age' ->
      noNegative weight >>= \weight' -> weightCheck (Cow name' age' weight')

-- Either
-- see EitherMonad.hs
{-
(>>=) :: Either e a -> (a -> Either e b) -> Either e b
return :: a -> Either e a
-}
--
-- Short Exercise: Either Monad
--
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second y) = Second (f y)

instance Applicative (Sum a) where
  pure = Second
  First x <*> _ = First x
  _ <*> First x = First x
  Second f <*> Second y = Second (f y)

instance Monad (Sum a) where
  return = pure
  First x >>= _ = First x
  Second y >>= f = f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return $ First a), (1, return $ Second b)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

runQB :: IO ()
runQB = do
  let trigger :: Sum (Int, String, Int) (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

--
--
-- 18.5 - Monad laws
--
-- Identity
{-
m >>= return      = m
return x >>= f    = f x
-}
--
-- Associativity
{-
(m >>= f) >>= g   = m >>= (\x -> f x >>= g)
-}
--
-- Test
{-
quickBatch (monad [(1, 2, 3)])
-}
--
-- BadMonad
-- see BadMonad.hs
--
--
-- 18.6 - Application and composition
--
-- Composition under functors just works:
{-
fmap ((+1) . (+2)) [1..5]      -- [4,5,6,7,8]
fmap (+1) . fmap (+2) $ [1..5] -- [4,5,6,7,8]
-}
--
mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = join (f <$> g a)

-- Same as:
mcomp' :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp' f g a = g a >>= f

-- Kleisli composition
sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM -- Oh, my.

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "
