module Chapter06.Scratch where

--
-- 6.5 - Writing typeclass instances
--
data Trivial =
  Trivial

-- Derive Eq manually (instead of automatically with `deriving`)
instance Eq Trivial where
  Trivial == Trivial = True

--
data Day
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Ord, Show) --
         -- ^^^ Note that the Eq instance defined below is required in order to
         -- allow the `deriving Ord` clause here, because Ord is a subclass of Eq

data Date =
  Date Day
       Int
  deriving (Show)

instance Eq Day where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  -- (==) _ _     = False

instance Eq Date where
  (==) (Date day1 date1) (Date day2 date2) = day1 == day2 && date1 == date2

--
-- Warning: "partial functions" will not provide a warning by default
f :: Int -> Bool
f 1 = True
f 2 = True
f _ = False -- Comment this line to see warning if using :set -Wall (reverse: -w)
            --                                        or :set -Wincomplete-patterns
            --                                   (reverse: -Wno-incomplete-patterns)

--
--
newtype Identity a =
  Identity a

-- Typeclass constraint needed on `a` here in order to rely on its Eq instance
instance Eq a => Eq (Identity a) where
  (==) (Identity v1) (Identity v2) = v1 == v2

--
-- 6.6 - Num
--
-- divideThenAdd :: Num a => a -> a -> a        <-- won't work with (/)
divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1

--
-- 6.10 - Show
--
data Mood =
  Blah

instance Show Mood where
  show _ = "Blah"

--
-- 6.12 - Instances are dispatched by type
--
class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a -- Don't do this

newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1988

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a1 a2 = fromNumber summed
  where
    integer1 = toNumber a1
    integer2 = toNumber a2
    summed = integer1 + integer2

--
-- 6.13 - Gimme more operations
--
-- add :: a -> a -> a                       -- Error
add :: Num a => a -> a -> a
add x y = x + y

-- addWeird :: Num a => a -> a -> a         -- Error
addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
  if x > 1
    then x + y
    else x

-- Concrete - no constraints needed
add' :: Int -> Int -> Int
add' x y = x + y

addWeird' :: Int -> Int -> Int
addWeird' x y =
  if x > 1
    then x + y
    else x

check' :: Int -> Int -> Bool
check' a a' = a == a'
