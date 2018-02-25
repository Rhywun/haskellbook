--
-- 2.5

-- 1
-- No change is required in the REPL.

-- 2
piTimesDouble x = 3.14 * x * x

-- 3
piTimesDouble' x = pi * x * x

--
-- 2.6

-- 1
-- Same

-- 2
-- Same

-- 3
-- Different

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

-- 1
-- 5

-- 2
-- 25

-- 3
-- 30

-- 4
-- 6

-- 1
f1 = x * 3 + y where x = 3; y = 1000

-- 2
f2 = x * 5 where y = 10; x = 10 * 5 + y

-- 3
f3 = z / x + y where x = 7; y = negate x; z = y * 10

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

-- More fun with functions

waxOn = x * 5 where z = 7; y = z + 8; x = y ^ 2

triple x = x * 3

waxOff x = triple x
