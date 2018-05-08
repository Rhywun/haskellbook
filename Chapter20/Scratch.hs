module Scratch where

import           Data.Foldable
import           Data.Monoid

--
-- 20.3 - Revenge of the monoids
--
--
-- `fold` requires a structure (e.g. list) of Monoids
-- fold :: (Monoid m, Foldable t) => t m -> m
--
-- Sometimes, we have to specify the Monoid explicitly - such as `Sum`:
-- (Remember, `Sum` here is the data constructor of type `Sum`)
rm01 = fold $ map Sum [1 .. 5] -- Sum {getSum = 15}

-- or `Product`:
rm02 = fold $ map Product [1 .. 5] -- Product {getProduct = 120}

-- Other times, the compiler can identify the standard Monoid for a type itself:
rm03 = fold ["hello", "julie"] -- "hellojulie"

-- `foldMap` specifies a function that explicitly maps to a Monoid
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
--
rm04 = foldMap Sum [1 .. 5] -- Sum {getSum = 15}

rm05 = foldMap Product [1 .. 5] -- Product {getProduct = 120}

rm06 = foldMap All [True, False, True] -- All {getAll = False}

rm07 = foldMap Any [3 == 4, 9 > 5] -- Any {getAny = True}

rm08 = foldMap First [Just 1, Nothing, Just 5] -- First {getFirst = Just 1}

rm09 = foldMap Last [Just 1, Nothing, Just 5] -- Last {getLast = Just 5}

-- We can also give it a function that's not the Monoid it's using:
--
rm10 = foldMap (* 5) $ map Product [1 .. 3] -- Product {getProduct = 750}
  -- i.e. (1 * 5) * (2 * 5) * (3 * 5)
  --         5    *    10   *    15

rm11 = foldMap (* 5) $ map Sum [1 .. 3] -- Sum {getSum = 30}, i.e. 5 + 10 + 15

-- Compare to `foldr`
-- The Monoid instance is baked in to the function
--
rm12 = foldr (*) 5 [1, 2, 3] -- 30
  -- i.e. 5 * (1 * 2 * 3)

-- In fact, an implied Monoid (e.g. Sum or Product) is ignored:
--
rm13 = foldr (*) 3 $ map Sum [2 .. 4] -- Sum {getSum = 72}
  -- i.e. 3 * (2 * 3 * 4)

rm14 = foldr (*) 3 $ map Product [2 .. 4] -- Product {getProduct = 72}
  -- same

-- Folding over one value also ignores an explicit Monoid instance:
-- (Note that the explicit instance is required to satisfy the type-checker)
--
rm15 = foldMap (* 5) (Just 100) :: Product Integer -- Product {getProduct = 500}

rm16 = foldMap (* 5) (Just 5) :: Sum Integer -- Sum {getSum = 25}

-- `mempty` comes into play when you're folding over something empty:
--
rm17 = foldMap (* 5) Nothing :: Sum Integer -- Sum {getSum = 0}

rm18 = foldMap (* 5) Nothing :: Product Integer -- Product {getProduct = 1}

--
-- 20.4 - Demonstrating Foldable instances
--
--
-- Identity
--
newtype Identity a =
  Identity a
  deriving (Eq, Show)

{-
foldr (*) 5 (Identity 5)                == 25
foldMap (* 5) (Identity 100) :: Sum Int == Sum {getSum = 500}
-}
instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

-- "Maybe"
--
data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)

{-
foldMap (+ 1) Nada :: Product Int == Product {getProduct = 1}
foldMap (+ 1) Nada :: Sum Int     == Sum {getSum = 0}
foldMap (+ 1) (Yep 2) :: Sum Int  == Sum {getSum = 3}
-}
instance Foldable Optional where
  foldr _ z Nada    = z
  foldr f z (Yep x) = f x z
  foldl _ z Nada    = z
  foldl f z (Yep x) = f z x
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

--
-- 20.5 - Some basic derived operations
--
--
toList1 = toList (Just 1) -- [1]

toList2 = map toList [Just 1, Just 2, Just 3] -- [[1],[2],[3]]

toList3 = concatMap toList [Just 1, Just 2, Just 3] -- [1,2,3]

toList4 = concatMap toList [Just 1, Just 2, Nothing] -- [1,2]

toList5 = toList (1, 2) -- [2]
  -- Remember, the first element of a 2-ple is part of the structure

null1 = null (Right 3) -- False

null2 = null (Left 3) -- True

null3 = null Nothing -- True

null4 = null (1, 2) -- False

null5 = fmap null [Just 1, Just 2, Nothing] -- False,False,True

length1 = length (1, 2) -- 1

length2 = length [(1, 2), (3, 4), (5, 6)] -- 3

length3 = fmap length [(1, 2), (3, 4), (5, 6)] -- [1,1,1]

length4 = fmap length Just [1, 2, 3] -- 1, because there is only 1 list

length4' = fmap length (Just [1, 2, 3]) -- Just 3

length5 = length $ Just [1, 2, 3] -- 1, same

length6 = fmap length [Just 1, Just 2, Just 3] -- [1,1,1]

length7 = fmap length [Just 1, Just 2, Nothing] -- [1,1,0]

elem1 = 2 `elem` Just 3 -- False

elem2 = True `elem` Left False -- False

elem3 = True `elem` Left True -- False, because remember Left is part of the structure

elem4 = True `elem` Right False -- False

elem5 = True `elem` Right True -- True

maximum1 = maximum [10, 12, 33, 5] -- 33

maximum2 = fmap maximum [Just 2, Just 10, Just 4] -- [2,10,4]
  -- Remember, we're mapping maximum to EACH Maybe - e.g. maximum (Just 2) == 2

maximum3 = fmap maximum (Just [3, 7, 10, 2]) -- Just 10

maximum3' = fmap maximum Just [3, 7, 10, 2] -- [3,7,10,2]

minimum1 = minimum "julie" -- 'e'

minimum2 = fmap minimum (Just "julie") -- Just 'e'

minimum3 = minimum <$> map Just "jul" -- "jul"
  -- Note: map Just "jul" == [Just 'j',Just 'u',Just 'l']

sum1 = sum (7, 5) -- 5

sum2 = fmap sum [(7, 5), (3, 4)] -- [5, 4]

sum3 = fmap sum (Just [1, 2, 3, 4, 5]) -- Just 15

product1 = product Nothing -- 1

product2 = fmap product (Just []) -- Just 1

product3 = fmap product (Right [1, 2, 3]) -- Right 6

--
-- Exercises: Library Functions
--
-- 1
{-
sum' [1,2,3] == 6
-}
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- 2
{-
product' [2,3,4] == 24
-}
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- 3 - PASS
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' = undefined

-- 4 - PASS
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = undefined

-- 5 - PASS
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = undefined

-- 6
{-
null' [] == True
null' (Left 3) == True
null' (Right 3) == False
-}
null' :: (Foldable t) => t a -> Bool
null' t = length t == 0

-- 7 - PASS
length' :: (Foldable t) => t a -> Int
length' = undefined

-- 8
{-
toList' (1,2) == [2]
toList' (Left 1) == []
toList' (Just 1) == [1]
-}
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- 9
{-
fold' ["hello", "julie"] == "hellojulie"
fold' $ map Sum [1 .. 5] == Sum {getSum = 15}
-}
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' = undefined
