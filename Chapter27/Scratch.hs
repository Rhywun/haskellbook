module Chapter27.Scratch where

--
-- 27.3 - Outside in, inside out
--

-- possiblyKaboom f = f fst snd (0, undefined)
possiblyKaboom = \f -> f fst snd (0, undefined)

true :: a -> a -> a
-- true a b = a
true = \a -> (\b -> a)

false :: a -> a -> a
-- false a b = b
false = \a -> (\b -> b)

oiio1 = possiblyKaboom true -- 0
oiio2 = possiblyKaboom false -- Exception

--
-- 27.4 - What does the other way look like?
--

hypo :: IO ()
hypo = do
  let x :: Int
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x -- Only a problem if we enter "hi"
    _    -> putStrLn "hello"

--
-- 27.5 - Can we make Haskell strict?
--

hypo' :: IO ()
hypo' = do
  let x :: Integer
      x = undefined
  s <- getLine
  case x `seq` s of -- `seq` causes x to be evaluated...
    "hi" -> print x
    _    -> putStrLn "hello" -- ...therefore, any entered value throws an Exception

-- Won't bottom out
notGonnaHappenBruh :: Int
notGonnaHappenBruh =
  let x = undefined
      y = 2
      z = (x `seq` y `seq` 10, 11)
  in  snd z

-- Core Dump
-- see CoreDump.hs
