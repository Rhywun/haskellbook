module Chapter18.Scratch where

import Control.Applicative
import Control.Monad

-- "Monads are applicative functors." Oh, boy.

-- In order of power:
-- Functor -> Application -> Monad

-- A law:
{-
fmap f xs = xs >>= return . f

fmap (+1) [1..3]            -- [2, 3, 4]
[1..3] >>= return . (+1)    -- [2, 3, 4]
-}

-- Core operations:
{-
(>>=) :: m a -> (a -> m b) -> m b     -- AKA `bind`
(>>) :: m a -> m b -> m b
return :: a -> m a                    -- <-- Same as `pure`
-}

-- The unique part of Monad:
{-
join :: Monad m => m (m a) -> m a
-}

-- Write `bind` in terms of `fmap` and `join`:

-- keep in mind this is (>>=) flipped                 -- (Wut?)
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f                                -- OK... lolcheat

-- `join` removes structure that was added by `fmap`

-- 18.3 - Do syntax and monads

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' = putStrLn "blah" >> putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' = putStrLn "blah" *> putStrLn "another thing"

--

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' = getLine >>= putStrLn         -- Looks like threading or piping to me...


-- cont. p. 1163
