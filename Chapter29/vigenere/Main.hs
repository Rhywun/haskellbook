module Main where

import Data.List (intercalate)
import           System.Environment             ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  putStr "You entered: "
  putStrLn $ intercalate ", " args
  return ()
