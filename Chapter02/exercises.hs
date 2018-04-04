module Chapter02.Exercises where

--
-- 2.5
--
-- 1
-- No change is required in the REPL for either, in my version of GHC.
--
-- 2
piTimesDouble x = 3.14 * x * x

-- 3
piTimesDouble' x = pi * x * x

--
-- 2.6
--
-- 1
-- Different
e020601 = 8 + 7 * 9 == (8 + 7) * 9 -- False

-- 2
-- Same
perimeter x y = (x * 2) + (y * 2)

perimeter' x y = x * 2 + y * 2

e020602 = perimeter 34 56 == perimeter' 34 56 -- True, at least for these inputs

-- 3
-- Different
f020603 x = x / 2 + 9

f020603' x = x / (2 + 9)

e020603 = f020603 534 == f020603' 534 -- False

--
-- 2.7
-- 1
area x = 3.14 * (x * x)

-- 2
double x = x * 2

-- 3
x = 7

y = 10

f = x + y

--
-- 2.10
--
-- 1
e021001 =
  let x = 5
  in x -- 5

-- 2
e021002 =
  let x = 5
  in x * x -- 25

-- 3
e021003 =
  let x = 5
      y = 6
  in x * y -- 30

-- 4
e021004 =
  let x = 3
      y = 1000
  in x + 3 -- 6

-- 1
f1 = x * 3 + y
  where
    x = 3
    y = 1000

-- 2
f2 = x * 5
  where
    y = 10
    x = 10 * 5 + y

-- 3
f3 = z / x + y
  where
    x = 7
    y = negate x
    z = y * 10

--
-- 2.11
-- Parenthesization
-- 1
b1a = 2 + 2 * 3 - 1

b1b = 2 + (2 * 3) - 1

-- 2
b2a = (^) 10 $ 1 + 1

b2b = (^) 10 (1 + 1)

-- 3
b3a = 2 ^ 2 * 4 ^ 5 + 1

b3b = ((2 ^ 2) * (4 ^ 5)) + 1

-- Equivalent expressions
-- Pairs 1 and 2 return the same result
--
-- More fun with functions
--
-- 1
--
waxOn = x * 5
  where
    z = 7
    y = z + 8
    x = y ^ 2

-- 10 + waxOn == 1135
-- (+10) waxOn == 1135
-- (-) 15 waxOn == -1110
-- (-) waxOn 15 == 1110

-- 2
triple x = x * 3
-- triple waxOn == 3375

waxOff = triple
-- waxOff waxOn == 3375
