module Chapter13.Exercises where

import           Control.Monad
import           Data.Char                      ( toLower )
import           System.Exit                    ( exitSuccess )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )

-- 1
-- See Chapter11/Ciphers.hs

-- 2, 3

palindrome :: IO ()
palindrome = forever $ do
  hSetBuffering stdout NoBuffering
  putStr "Palindrome? "
  input <- getLine
  let line1 = filter (\x -> x `elem` ['a' .. 'z']) $ map toLower input
  if line1 == reverse line1
    then putStrLn "It's a palindrome!"
    else do
      putStrLn "Nope!"
      exitSuccess

-- 4

type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0
  = Right $ Person name age
  | name == ""
  = Left NameEmpty
  | age <= 0
  = Left AgeTooLow
  | otherwise
  = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ "Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Name? "
  name <- getLine
  putStr "Age? "
  age <- getLine
  let person = mkPerson name (read age)
  case person of
    Left  NameEmpty                -> putStrLn "Name is empty"
    Left  AgeTooLow                -> putStrLn "Age is too low"
    Left  (PersonInvalidUnknown s) -> putStrLn ("Invalid person: " ++ s)
    Right _ -> putStrLn ("Yay! Successully got a person: " ++ show person)
-- FIX:
--      - Report multiple errors (technique was discussed earlier)
--      - Can't handle non-numeric age
--      - Handle backspace in input (this is shown in another example elsewhere)
--      - Q: How do we generate a PersonInvalidUnknown? I don't think it's possible
