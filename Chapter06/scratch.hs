--
-- 6.5 - Writing typeclass instances

data Trivial = Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

--

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Ord, Show)
                                                          -- ^^^ Note that the Eq instance below is
                                                          -- required in order to allow the Ord instance

data Date = Date Day Int deriving Show

instance Eq Day where
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _     = False

instance Eq Date where
    (==) (Date day date) (Date day' date') = day == day' && date == date'

--

-- Warning: "partial functions" will not provide a warning by default
f :: Int -> Bool
f 1 = True
f 2 = True
f _ = False                 -- Comment this line to see warning if using :set -Wall

-- Typeclass constraint

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

--
-- 6.6 - Num

-- divideThenAdd :: Num a => a -> a -> a
divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1

--
-- 6.10 - Show

data Mood = Blah

instance Show Mood where
    show _ = "Blah"

-- 6.12 - Instances are dispatched by type

class Numberish a where
    fromNumber    :: Integer -> a
    toNumber      :: a -> Integer
    defaultNumber :: a                          -- Don't do this

newtype Age = Age Integer deriving (Eq, Show)

instance Numberish Age where
    fromNumber n = Age n
    toNumber (Age n) = n
    defaultNumber = Age 65

newtype Year = Year Integer deriving (Eq, Show)

instance Numberish Year where
    fromNumber n = Year n
    toNumber (Year n) = n
    defaultNumber = Year 1988

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
    where integerOfA      = toNumber a
          integerOfAPrime = toNumber a'
          summed          = integerOfA + integerOfAPrime

-- 6.13 - Gimme more operations

-- add :: a -> a -> a                       -- Error
add :: Num a => a -> a -> a
add x y = x + y

-- addWeird :: Num a => a -> a -> a         -- Error
addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y = if x > 1 then x + y else x

-- Concrete - no constraints needed
add' :: Int -> Int -> Int
add' x y = x + y

addWeird' :: Int -> Int -> Int
addWeird' x y = if x > 1 then x + y else x

check' :: Int -> Int -> Bool
check' a a' = a == a'
