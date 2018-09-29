{-# LANGUAGE OverloadedStrings #-}

module Chapter24.Scratch where

import           Control.Applicative
import           Data.Ratio                     ( (%) )
import           Text.Trifecta

--
-- 24.3 - Understanding the parsing process
--

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = char '1'

-- read a single character '1', then die
one' = one >> stop

-- read two characters, '1' and '2'
oneTwo = char '1' >> char '2'

-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main1 = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

-- Exercises: Parsing Practice

oneEOF = one >> eof

main2 = do
  pNL "one eof should fail:"
  print $ parseString (one >> eof) mempty "123"
  pNL "oneTwo eof should fail:"
  print $ parseString (oneTwo >> eof) mempty "123"
  pNL "oneTwo eof should pass:"
  print $ parseString (oneTwo >> eof) mempty "12"
  --
  pNL "1 or 12 or 123 should pass:"
  print $ parseString (string "1" <|> string "12" <|> string "123") mempty "123"

--
-- 24.4 - Parsing fractions
--

-- Test inputs

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

-- Parser

fractionParser :: Parser Rational
fractionParser = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

main3 = do
  let parseFraction' = parseString fractionParser mempty
  print $ parseFraction' badFraction
  print $ parseFraction' alsoBad
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork

-- Exercise: Unit of Success
-- Totally lost here...

{-
yourFuncHere = do
  a <- integer
  b <- eof
  return a
-}

{-
parseString (yourFuncHere) mempty "123"    -- Success 123
parseString (yourFuncHere) mempty "123abc" -- Failure
-}

--
-- 24.6 - Alternative
--

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNumOrStr :: Parser NumberOrString
parseNumOrStr = (Left <$> integer) <|> (Right <$> some letter)

main4 = do
  let p f = parseString f mempty
  print $ p (some letter) a        -- Success "blah"
  print $ p integer b              -- Success 123
  print $ p parseNumOrStr a        -- Success (Right "blah")
  print $ p parseNumOrStr b        -- Success (Left 123)
  print $ p (many parseNumOrStr) c -- Success [Left 123,Right "blah",Left 789]
  print $ p (some parseNumOrStr) c -- Success [Left 123,Right "blah",Left 789]

--
-- 24.7 - Parsing configuration files
-- see Ini.hs
--
