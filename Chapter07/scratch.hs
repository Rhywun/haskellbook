module Scratch where

--
-- 7.2 - Arguments and parameters
--
myNum :: Num a => a
myNum = 1

myVal :: Num a => a -> a
myVal f = f + myNum

-- Shadowing:
-- bindExp 9001 == "x=10 and y=5"
bindExp :: Integer -> String
bindExp x -- <-- warning: [-Wunused-matches]
 =
  let x = 10
      y = 5
  in mconcat ["x=", show x, " and y=", show y]

--
-- 7.3 - Anonymous functions
--
{-
triple x =  x * 3     becomes:
      \x -> x * 3
-}
--
--
-- 7.4 - Pattern matching
--
isItTwo :: Integer -> Bool
-- isItTwo _ = False -- --> warning: [-Woverlapping-patterns]
isItTwo 2 = True
isItTwo _ = False
  -- if left out: "bottom" at runtime, or use :set -Wall or -Wincomplete-patterns
  -- to get a compiler warning

--
-- see RegisteredUser.hs
--
-- see Penguins.hs
--
-- see MatchingTuples.hs
--
--
-- 7.5 - Case expressions
--
-- rewriting this
-- if x + 1 == 1 then "AWESOME" else "wut"
funcZ x =
  case x + 1 == 1 of
    True  -> "AWESOME"
    False -> "wut"

pal xs =
  case xs == reverse xs of
    True  -> "yes"
    False -> "no"

pal' xs =
  case y of
    True  -> "yes"
    False -> "no"
  where
    y = xs == reverse xs

--
-- 7.6 - Higher-order functions
--
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

--
data Employee
  = Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank ::
     (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _     = GT
codersRuleCEOsDrool _ Coder     = LT
codersRuleCEOsDrool e e'        = compare e e'

-- e.g.
-- employeeRank codersRuleCEOsDrool Coder Manager
-- 7.7 - Guards
-- Rewrite this:
-- myAbs :: Integer -> Integer
-- myAbs x = if x < 0 then (-x) else x
myAbs :: Integer -> Integer
myAbs x
  | x < 0 = (-x)
  | otherwise = x

--
bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "too low"
  | x > 145 = "too high"
  | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | a ^ 2 + b ^ 2 == c ^ 2 = "RIGHT ON"
  | otherwise = "not right"

dogYrs :: Integer -> Integer
dogYrs x
  | x <= 0 = 0
  | x <= 1 = x * 15
  | x <= 2 = x * 12
  | x <= 4 = x * 8
  | otherwise = x * 6

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'E'
  where
    y = x / 100

-- 7.8 - Function composition
fc1 = negate . sum $ [1, 2, 3, 4, 5]

fc2 = take 5 . reverse $ [1 .. 10]

fc3 = take 5 . filter odd . enumFrom $ 3

-- 7.9 - Pointfree style
-- For some reason I don't understand, the first two require a type definition in the REPL
pf1 :: Num a => [a] -> a
pf1 = negate . sum

pf2 :: Num a => a -> [a] -> a
pf2 = foldr (+)

pf3 :: [Char] -> Int
pf3 = length . filter (== 'a')

-- 7.10 - Demonstrating compostion
print' a = putStrLn (show a)

print'' a = (putStrLn . show) a

print''' :: Show a => a -> IO () -- again, this is required
print''' = putStrLn . show

-- 7.12 - Chapter Definitions
data Blah =
  Blah

blahFunc :: Blah -> Bool
blahFunc Blah = True

--
data Identity a =
  Identity a
  deriving (Eq, Show)

unpackIdentity :: Identity a -> a
unpackIdentity (Identity x) = x

ignoreIdentity :: Identity a -> Bool
ignoreIdentity (Identity _) = True

ignoreIdentity' :: Identity a -> Bool
ignoreIdentity' _ = True

--
data Product a b =
  Product a
          b
  deriving (Eq, Show)

productUnpackOnlyA :: Product a b -> a
productUnpackOnlyA (Product x _) = x

productUnpackOnlyB :: Product a b -> b
productUnpackOnlyB (Product _ y) = y

productUnpack :: Product a b -> (a, b)
productUnpack (Product x y) = (x, y)

--
data SumOfThree a b c
  = FirstPossible a
  | SecondPossible b
  | ThirdPossible c
  deriving (Eq, Show)

sumToInt :: SumOfThree a b c -> Integer
sumToInt (FirstPossible _)  = 0
sumToInt (SecondPossible _) = 1
sumToInt (ThirdPossible _)  = 2

--
-- If you apply this to any values,
-- it'll recurse indefinitely.
f x = f x

-- It'll a'splode if you pass a False value
dontDoThis :: Bool -> Int
dontDoThis True = 1

-- morally equivalent to
definitelyDontDoThis :: Bool -> Int
definitelyDontDoThis True  = 1
definitelyDontDoThis False = error "oops"
-- don't use error.
-- We'll show you a better way soon.
