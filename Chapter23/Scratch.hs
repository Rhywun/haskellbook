module Chapter23.Scratch where

import           Control.Applicative            ( liftA3 )
import           Control.Monad                  ( replicateM )
import           Control.Monad.Trans.State
import           System.Random

--
-- 23.4 - The State newtype
--

{-
newtype State s a =
  State { runState :: s -> (a, s) }
-}

--
-- 23.5 - Throw down
--

data Die
  = Die1
  | Die2
  | Die3
  | Die4
  | Die5
  | Die6
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
  1 -> Die1
  2 -> Die2
  3 -> Die3
  4 -> Die4
  5 -> Die5
  6 -> Die6
  x -> errorWithoutStackTrace $ "Invalid Int: " ++ show x -- Don't do this in real code, see p. 1376

-- Suboptimal solution

roll3' :: (Die, Die, Die)
roll3' = do
  let s        = mkStdGen 0 -- Change this Int to get different rolls
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _ ) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

-- Better solution, with State

roll1' :: State StdGen Die
roll1' = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

-- Or, less verbosely

roll1 :: State StdGen Die
roll1 = intToDie <$> state (randomR (1, 6))

{-
evalState roll3 (mkStdGen 0) -- (Die6,Die6,Die4)
evalState roll3 (mkStdGen 1) -- (Die6,Die5,Die2)
-}
roll3 :: State StdGen (Die, Die, Die)
roll3 = liftA3 (,,) roll1 roll1 roll1

{-
evalState (rollN 5) (mkStdGen 0) -- [Die6,Die6,Die4,Die1,Die5]
-}
rollN :: Int -> State StdGen [Die]
rollN n = replicateM n roll1

-- Keep on rolling

{-
rollsToGet20 (mkStdGen 0)              -- 5
(rollsToGet20 . mkStdGen) <$> randomIO -- 9
(rollsToGet20 . mkStdGen) <$> randomIO -- 8
-}
rollsToGet20 :: StdGen -> Int
rollsToGet20 = go 0 0
 where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= 20
    = count
    | otherwise
    = let (die, nextGen) = randomR (1, 6) gen in go (sum + die) (count + 1) nextGen

-- Exercises: Roll Your Own

{-
rollsToGetN 100 (mkStdGen 0) -- 29
-}
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
 where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= n
    = count
    | otherwise
    = let (die, nextGen) = randomR (1, 6) gen in go (sum + die) (count + 1) nextGen

{-
rollsCountLogged 20 (mkStdGen 0) -- (5,[Die6,Die6,Die4,Die1,Die5])
-}
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 (0, [])
 where
  go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
  go sum (count, rolls) gen
    | sum >= n
    = (count, rolls)
    | otherwise
    = let (die, nextGen) = randomR (1, 6) gen
      in  go (sum + die) (count + 1, rolls ++ [intToDie die]) nextGen
  -- Cheated but I get it

--
-- 23.7 - Get a coding job with one weird trick
--

{-
mapM_ (putStrLn . fizzBuzz) [1..20]
-}
fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0  = "Buzz"
           | n `mod` 3 == 0  = "Fizz"
           | otherwise       = show n

{-
mapM_ putStrLn $ reverse $ fizzBuzzList [1..20]
  -- use package `dlist` and `snoc` if you want to avoid the `reverse` penalty
-}
fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []
 where
  addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

-- Fizzbuzz Differently

{-
mapM_ putStrLn $ fizzBuzzFromTo 1 20
-}
fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo start end = fizzBuzzList $ enumFromThenTo end (end - 1) start

-- NOTE
{-
  I don't understand why this works:
      mapM_ putStrLn $ fizzBuzzFromTo 1 20
  but this doesn't:
      mapM_ putStrLn $ fizzBuzzList [20..1] -- warning: [-Wempty-enumerations]
-}