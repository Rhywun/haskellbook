module Chapter09.Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar n xs = map (\x -> chr $ mod (ord x + n - a) 26 + a) xs
    where a = ord 'a'

uncaesar :: Int -> String -> String
uncaesar n = caesar (-n)
