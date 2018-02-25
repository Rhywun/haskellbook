module Chapter08.Exercises where

-- Review of types

e1n1 :: [[Bool]]
e1n1 = [[True, False], [True, True], [False, True]]

e1n2 :: [[Bool]]
e1n2 = [[3 == 3], [6 > 5], [3 < 4]]

func :: [a] -> [a] -> [a]
func x y = x ++ y
-- d

-- 4 -> b

-- Reviewing currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

e2n1 = appedCatty "woohoo!"
{-
cattyConny "woops" "woohoo!"
"woops mrow woohoo!"
-}

e2n2 = frappe "1"
{-
flippy "haha" "1"
flip cattyConny "haha" "1"
cattyConny "1" "haha"
"1 mrow haha"
-}

e2n3 = frappe (appedCatty "2")
{-
frappe (cattyConny "woops" "2")
frappe "woops mrow 2"
flippy "haha" "woops mrow 2"
flip cattyConny "haha" "woops mrow 2"
cattyConny "woops mrow 2" "haha"
"woops mrow 2 mrow haha"
-}

e2n4 = appedCatty (frappe "blue")
{-
appedCatty "blue mrow haha"
"woops mrow blue mrow haha"
-}

e2n5 = cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
{-
cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
cattyConny (frappe "pink") (cattyConny "green" "woops mrow blue")
cattyConny (flippy "haha" "pink") (cattyConny "green" "woops mrow blue")
cattyConny "pink mrow haha" (cattyConny "green" "woops mrow blue")
cattyConny "pink mrow haha" "green mrow woops mrow blue"
"pink mrow haha mrow green mrow woops mrow blue"
-}

e2n6 = cattyConny (flippy "Pugs" "are") "awesome"
{-
cattyConny (flippy "Pugs" "are") "awesome"
cattyConny (cattyConny "are" "Pugs") "awesome"
cattyConny "are mrow Pugs" "awesome"
cattyConny "are mrow Pugs mrow awesome"
-}

-- Recursion

-- 1

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy n d = go n d 0
    where go n' d' i | n' < d'   = (i, n')
                     | otherwise = go (n' - d') d' (i + 1)

{-
dividedBy 15 2 =
-> go 15 2 0
-> go (15 - 2) 2 (0 + 1)
-> go 13 2 1
-> go (13 - 2) 2 (1 + 1)
-> go 11 2 2
-> go (11 - 2) 2 (2 + 1)
-> go 9 2 3
-> go (9 - 2) 2 (3 + 1)
-> go 7 2 4
-> go (7 - 2) 2 (4 + 1)
-> go 5 2 5
-> go (5 - 2) 2 (5 + 1)
-> go 3 2 6
-> go (3 - 2) 2 (6 + 1)
-> go 1 2 7                             Finally n' < d'
-> (7, 1)
-}

-- 2

recsum :: (Eq a, Num a) => a -> a
recsum 1 = 1
recsum n = n + recsum (n - 1)

-- 3

mult :: (Integral a) => a -> a -> a
mult 0 _ = 0
mult x y = y + mult (x - 1) y

-- Fixing dividedBy

data DividedResult = Result Int Int | DividedByZero deriving Show

-- How to fix? I think:
---- take abs of both args
---- set the sign at the end

dividedBy' :: Int -> Int -> DividedResult
dividedBy' n d | d == 0               = DividedByZero
               | signum n == signum d = Result (fst r) (snd r)
               | otherwise            = Result (negate . fst $ r) (snd r)
    where r = go (abs n) (abs d) 0
          go n' d' i | n' < d'   = (i, n')
                     | otherwise = go (n' - d') d' (i + 1)

-- Nice! Cheated a little but I figured out how to handle returning both the div and the mod
---- Question: How do I return a tuple??

-- McCarthey 91 function

mc91 :: Int -> Int
mc91 n | n > 100   = n - 10
       | otherwise = mc91 (mc91 (n + 11))

-- Numbers into words
-- see WordNumber.hs
