module Main where

import qualified Data.Map                      as M
import           Morse
import           Test.QuickCheck

allowedChars :: String
allowedChars = M.keys morseMap

allowedMorse :: [Morse]
allowedMorse = M.elems morseMap

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain = forAll charGen (\c -> (charToMorse c >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck prop_thereAndBackAgain
