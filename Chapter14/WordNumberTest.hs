module Chapter14.WordNumberTest where

import           Test.Hspec
import           Data.List                      ( intercalate )

{-
digitToWord 1 -- "one"
-}
digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> undefined

{-
digits 123 -- [1,2,3]
-}
digits :: Int -> [Int]
digits n = go n []
 where
  go n' ds | n' < 10   = n' : ds
           | otherwise = go (n' `div` 10) (n' `mod` 10 : ds)

{-
wordNumber 12324546 -- "one-two-three-two-four-five-four-six"
-}
wordNumber :: Int -> String
wordNumber n = intercalate "-" (map digitToWord $ digits n)

--

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ digitToWord 1 `shouldBe` "one"
  describe "digits" $ do
    it "returns [1] for 1" $ digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ digits 100 `shouldBe` [1, 0, 0]
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zero-one for 9001" $ wordNumber 9001 `shouldBe` "nine-zero-zero-one"
