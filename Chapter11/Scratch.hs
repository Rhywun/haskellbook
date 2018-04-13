module Scratch where

import           Data.Int

--
-- 11.5 - Data constructors and values
--
data PugType =
  PugData

data HuskyType a =
  HuskyData
  -- a is a "phantom"

newtype DogueDeBordeaux doge =
  DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

{-
badDoge :: DogueDeBordeaux String
badDoge = DogueDeBordeaux 10
-}
--
data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

-- Exercises: Dog Types
{-
1. type constructor
2. * -> *
3. *
4. Num a => Doggies a
5. Doggies Integer
6. Doggies String
7. it is both
8. doge -> DogueDeBordeaux doge
9. DogueDeBordeaux String
-}
--
--
-- 11.6 - What's a type and what's data?
--
newtype Price = Price Integer deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Size
  = Small
  | Medium
  | Large
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer
        Price
  | Plane Airline
          Size
  deriving (Eq, Show)

-- Exercises: Vehicles
myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir Small

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- Fails when applied to a plane because the function is partial
-- 11.7 - Data constructor arities
data MyType =
  MyVal Int
  deriving (Eq, Show)

-- Exercises: Cardinality
{-
1. PugType - 1
2. Airline - 3
3. Int16 - 65,534
4. Int has a finite bound; Integer has no bound
5. 2 ^ 8 = 256; equivalently, 8 is the number of bits representing an Int8
-}
-- Simple datatypes with nullary data constructors
data Example =
  MakeExample
  deriving (Show)

-- Exercises - For Example
{-
1. MakeExample :: Example
2. Yes, the REPL shows that Example has an instance of Show
-}
data Example2 =
  MakeExample2 Int
  deriving (Show)

{-
3. MakeExample2 :: Int -> Example2
   The type resembles the type of a fuction taking an Int
-}
-- 11.9 - newtype
newtype Goats =
  Goats Int
  deriving (Eq, Show)

newtype Cows =
  Cows Int
  deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

--
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- Need to explicitly assign Int to a
-- e.g. tooMany (42 :: Int)
instance TooMany Goats where
  tooMany (Goats n) = n > 43

-- No need to explicitly assign type
-- e.g. tooMany (Goats 44)
-- Exercises: Logic Goats
-- See LogicGoats.hs
-- 11.10 - Sum types
cardinalityOfBool = length $ enumFrom False

-- Exercises: Pity the Bool
{-
data BigSmall = Big Bool | Small Bool deriving (Eq, Show)
-- 2 + 2 = 4
-}
data NumberOrBool
  = Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

-- 256 + 2 = 258
-- 11.11 - Product types
data QuantumBool
  = QuantumTrue
  | QuantumFalse
  | QuantumBoth
  deriving (Eq, Enum, Show)

cardinalityOfQuantumBool = length $ enumFrom QuantumTrue

-- 3
data TwoQs =
  MkTwoQs QuantumBool
          QuantumBool
  deriving (Eq, Show)

-- cardinality is # * 3 = 9
type TwoQs' = (QuantumBool, QuantumBool)

-- Record syntax
-- without
data Person' =
  MkPerson String
           Int
  deriving (Eq, Show)

-- sample data
jm = MkPerson "julie" 108

ca = MkPerson "chris" 16

namae :: Person' -> String
namae (MkPerson s _) = s

data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Eq, Show)

papu = Person "Papu" 5

-- now we get these methods for free
a = age papu

n = name papu

-- 11.2 -- Normal form
{-
data TFiction = Fiction deriving Show
data TNonfiction = Nonfiction deriving Show
data TBook = FictionBook TFiction | NonfictionBook TNonfiction deriving Show

type AuthorName = String
data TAuthor = Author (AuthorName, TBook)       -- Not normal form
-}
type AuthorName = String

data TAuthor
  = Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)

-- "Normal form" is a sum of products
--
data Expr
  = Number Int
  | Add Expr
        Expr
  | Minus Expr
  | Mult Expr
         Expr
  | Divide Expr
           Expr

-- Exercises: How Does Your Garden Grow?
{-
data TFlower = Gardenia | Daisy | Rose | Lilac deriving Show
type Gardener = String
data TGarden = Garden Gardener TFlower deriving Show
-}
type Gardener = String

data TGarden
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving (Show)

-- 11.13 Constructing and deconstructing values
-- Products
data Product a b =
  Product a
          b
  deriving (Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow
            NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow
               NumPig
               NumSheep
  deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

-- Sums
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

type Name = String

type Age = Int

type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name
          Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name
          Age
          LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name
            Age
            PoundsOfWool
  deriving (Eq, Show)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

-- Values
type Awesome = Bool

data GuessWhat =
  Chickenbutt
  deriving (Eq, Show)

trivialValue :: GuessWhat
trivialValue = Chickenbutt

data Id a =
  MkId a
  deriving (Eq, Show)

idInt :: Id Integer
idInt = MkId 10

person :: Product Name Awesome
person = Product "Simon" True

data Twitter =
  Twitter
  deriving (Eq, Show)

data AskFm =
  AskFm
  deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

--
data RecordProduct a b = RecordProduct
  { pfirst  :: a
  , psecond :: b
  } deriving (Eq, Show)

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.00001

myRecord' :: RecordProduct Integer Float
myRecord' = RecordProduct {pfirst = 42, psecond = 0.00001}

-- Domain
data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer = Programmer
  { os   :: OperatingSystem
  , lang :: ProgLang
  } deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer {os = Mac, lang = Haskell}

feelingWizardly :: Programmer
feelingWizardly = Programmer {lang = Agda, os = GnuPlusLinux}

-- Exercise: Programmers
allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers =
  [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

-- Accidental bottoms from records
{-
-- warning: [-Wmissing-fields]
partialAf = Programmer { os = Mac }
-}
-- Works the same as if we'd used record syntax
data ThereYet =
  There Float
        Int
        Bool
  deriving (Eq, Show)

-- who needs a "builder pattern"?
nope :: Float -> Int -> Bool -> ThereYet
nope = undefined

notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yusssss :: ThereYet
yusssss = notQuite False

-- Not I, said the Haskell user.
-- Deconstructing values
newtype FarmerName =
  FarmerName String
  deriving (Show)

newtype Acres =
  Acres Int
  deriving (Show)

data FarmerType
  = DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving (Show)

data Farmer =
  Farmer FarmerName
         Acres
         FarmerType
  deriving (Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _                        = False

data FarmerRec = FarmerRec
  { farmerName :: FarmerName
  , acres      :: Acres
  , farmerType :: FarmerType
  } deriving (Show)

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _           -> False

-- 11.14 - Function type is exponential
data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

-- Exercises: The Quad
{-
1. 8
2. 16
3. 256
4. 8
5. 16
6. 65,536
-}
-- 11.16 - Lists are polymorphic
data List a
  = Nil
  | Cons a
         (List a)

nil = Nil

oneItem = (Cons "W00t!" Nil)

-- 11.17 - Binary Tree
data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

-- Exercises
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf                = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

--
preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears."

--
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf                = b
foldTree f b (Node left a right) = foldTree f (f a (foldTree f b left)) right
