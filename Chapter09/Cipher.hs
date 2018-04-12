module Cipher where

import           Data.Char

caesar :: Int -> String -> String
caesar n = map (\ x -> chr $ mod (ord x + n - a) 26 + a)
  where
    a = ord 'a'

uncaesar :: Int -> String -> String
uncaesar n = caesar (-n)
