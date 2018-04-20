module Addition where

import           Test.Hspec
import           Test.QuickCheck

--
-- 14.3 - Conventional testing
--
-- Test this
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy n d = go n d 0
  where
    go n' d' i
      | n' < d' = (i, n')
      | otherwise = go (n' - d') d' (i + 1)


-- Intermission: Short Exercise - test this
mult :: Int -> Int -> Int
mult 0 _ = 0
mult x y = y + mult (x - 1) y

--
-- 14.4 - Enter QuickCheck
--
-- Define our own generators
--
trivialInt :: Gen Int
trivialInt = return 1

oneToThree :: Gen Int
oneToThree = elements [1, 2, 3]

oneToThree' :: Gen Int
oneToThree' = elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

gen2ple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
gen2ple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

gen3ple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
gen3ple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- Equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- What QuickCheck does so you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return (Just a))]

-- Using QuickCheck without Hspec
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

--
main :: IO ()
main =
  hspec $ do
    describe "Addition" $ do
      it "1 + 1 is greater than 1" $ ((1 :: Int) + 1) > 1 `shouldBe` True
      it "2 + 2 is equal to 4" $ (2 :: Int) + 2 `shouldBe` 4
    describe "Division" $ do
      it "15 divided by 3 is 5" $ dividedBy (15 :: Int) 3 `shouldBe` (5, 0)
      it "22 divided by 5 is 4 remainder 2" $
        dividedBy (22 :: Int) 5 `shouldBe` (4, 2)
    describe "Multiplication" $ it "10 times 2 is 20" $ mult 10 2 `shouldBe` 20
    describe "QuickCheck Property" $
      it "x + 1 is always greater than x" $ property $ \x -> x + 1 > (x :: Int)
