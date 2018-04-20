module Main where

import qualified Data.Map        as Map
import           Morse
import           Test.QuickCheck

allowedChars :: String
allowedChars = Map.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = Map.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll charGen (\c -> (charToMorse c >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck prop_thereAndBackAgain
