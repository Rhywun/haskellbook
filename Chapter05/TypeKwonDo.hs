module TypeKwonDo where

-- 0
--
data Woot

data Blah

f0 :: Woot -> Blah
f0 = undefined

g0 :: (Blah, Woot) -> (Blah, Blah)
g0 (b, w) = (b, f0 w)

-- 1
--
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h n = g (f n)

-- 2
--
data A

data B

data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w (q a)

-- 3
--
data X

data Y

data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4
--
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xy ywz x = fst $ ywz $ xy x
