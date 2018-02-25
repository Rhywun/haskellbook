import Data.List (sort)

-- 6.5 - Eq Instances

data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where (==) (TisAn i) (TisAn j) = i == j

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where (==) (Two i j) (Two i' j') = i == i' && j == j'

-- Assuming here that TisAnInt 1 == TisAString "1"
data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt i) (TisAnInt j) = i == j
    (==) (TisAString s) (TisAString t) = s == t
    (==) (TisAnInt i) (TisAString s) = show i == s
    (==) (TisAString s) (TisAnInt i) = s == show i

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where (==) (Pair x y) (Pair x' y') = x == x' && y == y'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

-- Assuming here that ThisOne 1 == ThatOne 1
data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThatOne y) = x == y
    (==) (ThatOne x) (ThisOne y) = x == y
    (==) (ThisOne x) (ThisOne y) = x == y
    (==) (ThatOne x) (ThatOne y) = x == y

-- Assuming here that Hello 1 /= Goodbye 1
data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello y) = x == y
    (==) (Goodbye x) (Goodbye y) = x == y
    (==) _ _ = False

-- 6.8 - Will They Work?

e81 = max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
-- Yes, because boths arguments are Int and Int has an instance of Ord

e82 = compare (3 * 4) (3 * 5)
-- Yes, because boths arguments are Int and Int has an instance of Ord

-- e83 = compare "Julie" True
-- No, because both arguments are not [Char]

e84 = (5 + 3) > (3 + 6)
-- Yes, because boths arguments are Int and Int has an instance of Ord

-- 6.14

-- Multiple choice

{-
    1c, 2b, 3a, 4c, 5a
-}

-- Does it typecheck?

data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah | Woot deriving (Eq, Show)
settleDown x = if x == Woot then Blah else x

type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool"            -- s1 is a function that takes an Object
s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do?

data Rocks = Rocks String   deriving (Eq, Ord, Show)
data Yeah = Yeah Bool       deriving (Eq, Ord, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Ord, Show)

phew = Papu (Rocks "chases") (Yeah True)
truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

-- Match the types

-- 1

i :: Num a => a
-- i :: a                   -- this fails because 1 is inferred to be Num
i = 1

-- 2

f2 :: Float
-- f2 :: Num a => a         -- this fails because 1.0 is inferred to be Fractional
f2 = 1.0

-- 3

-- f3 :: Float
f3 :: Fractional a => a     -- same as above
f3 = 1.0

-- 4

-- f4 :: Float
f4 :: RealFrac a => a       -- this works because Float has an instance of RealFrac
f4 = 1.0

-- 5

-- freud :: a -> a
freud :: Ord a => a -> a    -- this works because in the inferred type p -> p, p can be any type
freud x = x

-- 6

-- freud' :: a -> a
freud' :: Int -> Int        -- same as above
freud' x = x

-- 7

myX = 1 :: Int

sigmund :: Int -> Int
-- sigmund :: a -> a        -- this fails because x is bound to type a, and myX is already bound to
                            -- concrete type Int (not a) - the types don't match
sigmund x = myX

-- 8

sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a        -- same as above
sigmund' x = myX

-- 9

-- jung :: Ord a => [a] -> a
jung :: [Int] -> Int                -- this works because Int has an instance of Ord
jung xs = head (sort xs)

-- 10

-- young :: [Char] -> Char
young :: Ord a => [a] -> a          -- this is already what is inferred, because sort requires Ord
young xs = head (sort xs)

-- 11

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
-- signifier :: Ord a => [a] -> a       -- this fails because xs is bound to type [a], and the return
--                                      -- value is already bound to concrete type Char (not a) - the
                                        -- types don't match
signifier xs = head (mySort xs)

-- Type-Kwon-Do Two: Electric Typealoo

-- 1

-- chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b                                    -- cheated

-- 2

-- arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n a = f a + fromInteger n                       -- cheated
