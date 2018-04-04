module Scratch where

triple x = x * 3

multi = x * y
  where
    x = 5
    y = 6

multi' = x * 3 + y
  where
    x = 3
    y = 1000

multi'' = x * 5
  where
    y = 10
    x = 10 * 5 + y

multi''' = z / x + y
  where
    x = 7
    y = negate x
    z = y * 10

waxOn = x * 5
  where
    z = 7
    y = z + 8
    x = y ^ 2

waxOff x = triple x
