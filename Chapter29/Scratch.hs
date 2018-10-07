module Chapter29.Scratch where

import           Control.Monad
import           Data.Time.Calendar
import           Data.Time.Clock
import           System.Random


--
-- 29.7 - IO's Functor, Applicative, and Monad
--

-- Functor

{-
> e1
6805743621684506711
> e1
8869577191291690297
-}
e1 :: IO Int
e1 = fmap (+ 1) (randomIO :: IO Int)

-- Applicative

{-
> e2
joe
blow
"joeblow"
-}
e2 :: IO String
e2 = (++) <$> getLine <*> getLine

{-
> e3
4260283203931281104
> e3
-6397604043598776980
-}
e3 :: IO Int
e3 = (+) <$> (randomIO :: IO Int) <*> (randomIO :: IO Int)

-- Monad

embedInIO :: a -> IO a
embedInIO = return

s = "I'll put in some ingredients"

e4 :: IO ()
e4 = join $ embedInIO $ print s

-- nested IO
{-
-- Today is an odd-numbered day:
> blah <- huehue
> either (>>= print) id blah
no soup for you
-}
huehue :: IO (Either (IO Int) (IO ()))
huehue = do
  t <- getCurrentTime
  let (_, _, dayOfMonth) = toGregorian (utctDay t)
  case even dayOfMonth of
    True  -> return $ Left randomIO
    False -> return $ Right (putStrLn "no soup for you")

