module Main where

import           Lib
import           DogsRule
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Name? "
  name <- getLine
  sayHello name
  dogs
