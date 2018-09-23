module Chapter14.UsingQuickCheck where

import           Data.Char                      ( toUpper )
import           Data.Int                       ( Int8 )
import           Data.List                      ( sort )
import           Test.QuickCheck

-- 1

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (* 2) . half

{-
quickCheck prop_halfIdentity -- +++ OK, passed 100 tests.
-}
prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = halfIdentity x == x

-- 2

listOrdered :: Ord a => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
 where
  go _ status@(_      , False) = status
  go y (       Nothing, t    ) = (Just y, t)
  go y (       Just x , t    ) = (Just y, x >= y)

{-
quickCheck prop_listOrdered -- +++ OK, passed 100 tests.
-}
prop_listOrdered :: Ord a => [a] -> Bool
prop_listOrdered xs = listOrdered $ sort xs

-- 3

{-
quickCheck prop_plusAssociative -- +++ OK, passed 100 tests.
-}
prop_plusAssociative :: (Eq a, Integral a) => a -> a -> a -> Bool
prop_plusAssociative x y z = x + (y + z) == (x + y) + z

{-
quickCheck prop_plusCommutative -- +++ OK, passed 100 tests.
-}
prop_plusCommutative :: (Eq a, Integral a) => a -> a -> Bool
prop_plusCommutative x y = x + y == y + x

-- 4

{-
quickCheck prop_timesAssociative -- +++ OK, passed 100 tests.
-}
prop_timesAssociative :: (Eq a, Integral a) => a -> a -> a -> Bool
prop_timesAssociative x y z = x * (y * z) == (x * y) * z

{-
quickCheck prop_timesCommutative -- +++ OK, passed 100 tests.
-}
prop_timesCommutative :: (Eq a, Integral a) => a -> a -> Bool
prop_timesCommutative x y = x * y == y * x

-- 5

-- Make sure y can't be 0
genIntsGTZero :: Gen Int8
genIntsGTZero = elements [1 ..]

{-
quickCheck prop_quotRemLaw -- +++ OK, passed 100 tests.
-}
prop_quotRemLaw :: Property
prop_quotRemLaw = forAll genIntsGTZero (\y x -> quot x y * y + rem x y == x)

{-
quickCheck prop_divModLaw -- +++ OK, passed 100 tests.
-}
prop_divModLaw :: Property
prop_divModLaw = forAll genIntsGTZero (\y x -> div x y * y + mod x y == x)

-- 6

{-
quickCheck prop_powAssociative -- *** Failed! Falsifiable (after 1 test)
-}
prop_powAssociative :: (Eq a, Num a, Integral b2, Integral b1) => a -> b2 -> b1 -> Bool
prop_powAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

{-
quickCheck prop_powCommutative -- *** Failed! Falsifiable (after 3 tests and 1 shrink)
-}
prop_powCommutative :: Integral b => b -> b -> Bool
prop_powCommutative x y = x ^ y == y ^ x

-- 7

{-
quickCheck prop_reverseListId -- +++ OK, passed 100 tests.
-}
prop_reverseListId :: (Eq a, Num a) => [a] -> Bool
prop_reverseListId xs = (reverse . reverse) xs == id xs

-- 8

{-
quickCheck prop_funcApplication -- +++ OK, passed 100 tests.
-}
prop_funcApplication :: Char -> Bool
prop_funcApplication c = (toUpper $ c) == toUpper c

{-
quickCheck prop_funcComposition -- +++ OK, passed 100 tests.
-}
prop_funcComposition :: Double -> Bool
prop_funcComposition n = (square . abs) n == (\x -> square (abs x)) n

-- 9

{-
quickCheck prop_foldr1 -- +++ OK, passed 100 tests.
-}
prop_foldr1 :: [Int] -> Bool
prop_foldr1 xs = foldr (:) [] xs == (++) [] xs

{-
quickCheck prop_foldr2 -- +++ OK, passed 100 tests.
-}
prop_foldr2 :: [String] -> Bool
prop_foldr2 xs = foldr (++) [] xs == concat xs

-- 10

-- Use positive integers and non-empty list
{-
quickCheck prop_lengthTake -- *** Failed! Falsifiable
    -- E.g. prop_lengthTake 2 ['c']
-}
prop_lengthTake :: Positive Int -> NonEmptyList Char -> Bool
prop_lengthTake (Positive n) (NonEmpty xs) = length (take n xs) == n

-- 11

{-
quickCheck prop_readShow -- +++ OK, passed 100 tests.
-}
prop_readShow :: Integer -> Bool
prop_readShow x = read (show x) == (x :: Integer)

{-
quickCheck prop_readShow' -- +++ OK, passed 100 tests.
-}
prop_readShow' :: String -> Bool
prop_readShow' x = read (show x) == (x :: String)

--
-- Failure
--

-- This fails because floating point arithmetic introduces precision errors
-- E.g. square (sqrt 2.0) = 2.0000000000000004
square :: Double -> Double
square x = x * x

prop_squareIdentity :: Double -> Bool
prop_squareIdentity x = (square . sqrt) x == x

--
-- Idempotence
--

twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

-- 1

capitalizeWord :: String -> String
capitalizeWord []       = []
capitalizeWord (c : cs) = toUpper c : cs

{-
qc prop_idemCap -- +++ OK, passed 100 tests.
-}
prop_idemCap :: String -> Bool
prop_idemCap x =
  (capitalizeWord x == twice capitalizeWord x)
    && (capitalizeWord x == fourTimes capitalizeWord x)

-- 2

{-
qc prop_idemSort -- +++ OK, passed 100 tests.
-}
prop_idemSort :: [Int] -> Bool
prop_idemSort xs = (sort xs == twice sort xs) && (sort xs == fourTimes sort xs)

--
-- Make a Gen random generator for the datatype
--

-- 1

data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

{-
sample' genFool
  -- [Fulse,Frue,Frue,Frue,Frue,Frue,Fulse,Frue,Frue,Frue,Frue]
-}
genFool = elements [Fulse, Frue]

-- 2

{-
sample' genFool'
  -- [Fulse,Frue,Fulse,Frue,Fulse,Fulse,Fulse,Fulse,Fulse,Fulse,Fulse]
-}
genFool' = frequency [(2, return Fulse), (1, return Frue)]
