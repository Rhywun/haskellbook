module Main where

import           Control.Exception
import           Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
  putStrLn ("Error: " ++ show e)
  writeFile "bbb" "hi" -- <-- We give an alternate file to write to

main = do
  -- Write to an unprotected file - no problem!
  {-
  writeFile "aaa" "hi"
  putStrLn "wrote to file"
  -}

  -- Try to write to a protected file - throws exception
  writeFile "zzz" "hi" `catch` handler
