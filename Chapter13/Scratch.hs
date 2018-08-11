module Chapter13.Scratch where

-- Intermission: Check your understanding
--
{-
1. forever, when
2. Data.Bit, Database.Blacktip.Types
3. typeclasses
4a. Control.Concurrent.Mvar, Filesystem.Path.CurrentOS, Control.Concurrent
 b. Filesystem
 c. Control.Monad
-}
--
-- `return` puts a value into a Monad so it can be returned from IO
--
twoo :: IO Bool
twoo = do
  c <- getChar
  c' <- getChar
  return (c == c')

main :: IO ()
main = do
  c <- getChar
  c' <- getChar
  if c == c'
    then putStrLn "True"
    else return ()
