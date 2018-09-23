module Addition where

import           Test.Hspec
import           Test.QuickCheck

--
-- 14.3 - Conventional testing
--

-- Test this
{-
dividedBy 15 3 -- (5,0)
dividedBy 22 5 -- (4,2)
-}
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy n d = go n d 0
 where
  go n' d' i | n' < d'   = (i, n')
             | otherwise = go (n' - d') d' (i + 1)

-- Intermission: Short Exercise - test this
{-
mult 10 2 -- 20
-}
mult :: Int -> Int -> Int
mult 0 _ = 0
mult x y = y + mult (x - 1) y

--
-- 14.4 - Enter QuickCheck
--

-- Define our own generators

{-
sample' trivialInt -- [1,1,1,1,1,1,1,1,1,1,1]
-}
trivialInt :: Gen Int
trivialInt = return 1

{-
sample' oneToThree -- [1,2,1,3,2,2,3,3,2,3,3]
-}
oneToThree :: Gen Int
oneToThree = elements [1, 2, 3]

{-
sample' oneToThree' -- [1,2,1,2,2,1,2,2,2,3,2]
-}
oneToThree' :: Gen Int
oneToThree' = elements [1, 2, 2, 2, 2, 3]

{-
sample' genBool -- [True,True,True,False,True,False,False,True,False,False,False]
-}
genBool :: Gen Bool
genBool = choose (False, True)

{-
sample' genBool' -- [False,True,True,True,True,False,True,True,True,True,True]
-}
genBool' :: Gen Bool
genBool' = elements [False, True]

{-
sample' genOrdering -- [EQ,LT,EQ,LT,GT,LT,GT,EQ,GT,EQ,LT]
-}
genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

{-
sample' genChar -- "etpbhqgzwgi"
-}
genChar :: Gen Char
genChar = elements ['a' .. 'z']

genChar' :: Gen Char
genChar' = choose ('A', 'Z')

{-
sample' gen2ple'
  -- [(0,0.0),(-2,4.528139e-2),(-3,0.8527228),(-5,-6.0497985),(2,-9.78754),...]
-}
gen2ple' :: Gen (Int, Float)
gen2ple' = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

-- Generators with polymorphic type arguments

{-
sample' (gen2ple :: Gen ([()], Char))
  -- [([],'q'),([(),()],'\517654'),([],'\911552'),([(),()],'\ESC'),...]
-}
gen2ple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
gen2ple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

{-
sample' (gen3ple :: Gen ([Int], Float, String))
  -- [([],0.0,""),([],-5.243311,"?V"),([-4,3],10.907302,"\686908\1050159\464497"),...]
-}
gen3ple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
gen3ple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

{-
sample' (genEither :: Gen (Either Int Char))
-- [Right '\DC1',Left 0,Right '\CAN',Left (-5),Right 'n',Left 9,Right '=',Right 'h',...]
-}
genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- Equal probability
{-
sample' (genMaybe :: Gen (Maybe Int))
  -- [Just 0,Nothing,Just 1,Just 1,Just 5,Nothing,Just 10,Just (-6),Just 10,Nothing,Nothing]
-}
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- How to increase the ratio of Just values to Nothing values
{-
sample' (genMaybe' :: Gen (Maybe Int))
  -- [Nothing,Nothing,Just 1,Just 4,Just 5,Nothing,Just 7,Just (-6),Nothing,Just (-2),Just (-4)]
-}
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return (Just a))]

-- Using QuickCheck without Hspec

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

--

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ ((1 :: Int) + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ (2 :: Int) + 2 `shouldBe` 4
  describe "Division" $ do
    it "15 divided by 3 is 5" $ dividedBy (15 :: Int) 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ dividedBy (22 :: Int) 5 `shouldBe` (4, 2)
  describe "Multiplication" $ it "10 times 2 is 20" $ mult 10 2 `shouldBe` 20
  describe "QuickCheck Property" $ it "x + 1 is always greater than x" $ property $ \x ->
    x + 1 > (x :: Int)
