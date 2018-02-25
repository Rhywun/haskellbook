module Chapter14.UsingQuickCheck where

import Data.Char       (toUpper)
import Data.Int        (Int8)
import Data.List       (sort)
import Test.QuickCheck

-- 1

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (* 2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = halfIdentity x == x

-- 2

listOrdered :: Ord a => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t)      = (Just y, t)
          go y (Just x, t)       = (Just y, x >= y)

prop_listOrdered :: Ord a => [a] -> Bool
prop_listOrdered xs = listOrdered $ sort xs

-- 3

prop_plusAssociative x y z = x+(y+z)==(x+y)+z
prop_plusCommutative x y = x+y==y+x

-- 4

prop_timesAssociative x y z = x * (y * z) == (x * y) * z
prop_timesCommutative x y = x * y == y * x

-- 5
-- Make sure y can't be 0

genIntsGTOne :: Gen Int8
genIntsGTOne = elements [1..]

quotRemLaw :: Property
quotRemLaw = forAll genIntsGTOne (\y x -> (quot x y) * y + (rem x y) == x)

divModLaw :: Property
divModLaw = forAll genIntsGTOne (\y x -> (div x y) * y + (mod x y) == x)

-- 6

prop_powAssociative x y z = (x^y)^z == x^(y^z)
prop_powCommutative x y = x^y == y^x

-- 7

prop_reverseListId xs = (reverse . reverse) xs == id xs

-- 8
-- Wut?

-- 9
-- Um...

-- 10

prop_lengthTake n xs = length (take n xs) == n
-- Fails when xs = []

-- 11

prop_readShow x = (read (show x)) == (x :: Integer)
prop_readShow' x = (read (show x)) == (x :: String)

-- Failure
-- This fails because floating point arithmetic introduces precision errors
-- E.g. square (sqrt 2.0) = 2.0000000000000004

square :: Double -> Double
square x = x * x

prop_squareIdentity :: Double -> Bool
prop_squareIdentity x = (square . sqrt) x == x

-- Idempotence

twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

-- 1

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (c:cs) = toUpper c : cs

prop_idemCap x = (capitalizeWord x == twice capitalizeWord x)
                 && (capitalizeWord x == fourTimes capitalizeWord x)

-- 2

prop_idemSort :: [Int] -> Bool
prop_idemSort xs = (sort xs == twice sort xs) && (sort xs == fourTimes sort xs)

-- Make a Gen random generator for the datatype

-- 1

data Fool = Fulse | Frue deriving (Eq, Show)

genFool = elements [Fulse, Frue]

-- 2

genFool' = frequency [(2, return Fulse), (1, return Frue)]
