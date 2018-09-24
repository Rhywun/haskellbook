module Chapter22.Scratch where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Char

--
-- 22.2 - A new beginning
--

boop :: Integer -> Integer
boop = (* 2)

doop :: Integer -> Integer
doop = (+ 10)

{-
bip 9 -- 38, i.e. boop (doop 9)
-}
bip :: Integer -> Integer
bip = boop . doop

{-
bloop 9 -- 38
-}
bloop :: Integer -> Integer
bloop = fmap boop doop
  -- The functor of functions, or "lifting over a partially-applied function"

--

{-
bbop 9 -- 37, i.e. boop 9 + doop 9
-}
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

{-
duwop 9 -- 37
-}
duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

--

{-
boopDoop 9 -- 37, same as the applicative example (bbop) but monadic
-}
boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

-- Short Exercise: Warming Up

cap :: String -> String
cap xs = map toUpper xs

rev :: String -> String
rev xs = reverse xs

{-
composed "Julie" -- "EILUJ"
-}
composed :: String -> String
composed = cap . rev

{-
fmapped "Julie" -- "EILUJ"
-}
fmapped :: String -> String
fmapped = fmap cap rev

{-
tupled "Julie" -- ("JULIE","eiluJ")
-}
tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

{-
tupled' "Julie" -- ("eiluJ","JULIE")
-}
tupled' :: String -> (String, String)
tupled' = (,) <$> rev <*> cap

{-
tupledM "Julie" -- ("eiluJ","JULIE")
-}
tupledM :: String -> (String, String)
tupledM = do
  a <- rev
  b <- cap
  return (a, b)

{-
tupledM' "Julie" -- ("eiluJ","JULIE")
-}
tupledM' :: String -> (String, String)
tupledM' = rev >>= \a -> cap >>= \b -> return (a, b)

--
-- 22.5 - But uh, Reader?
--

-- Exercise: Ask

ask :: Reader a a
ask = undefined -- something to do with `id` I think

--
-- 22.6 - Functions have an Applicative too
--

newtype HumanName
  = HumanName String
  deriving (Eq, Show)

newtype DogName
  = DogName String
  deriving (Eq, Show)

newtype Address
  = Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- without Reader
{-
getDog pers -- Dog {dogsName = DogName "Barkley", dogsAddress = Address "Sesame Street"}
-}
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
{-
getDogR pers -- Dog {dogsName = DogName "Barkley", dogsAddress = Address "Sesame Street"}
-}
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- or
{-
getDogR' pers -- Dog {dogsName = DogName "Barkley", dogsAddress = Address "Sesame Street"}
-}
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address
