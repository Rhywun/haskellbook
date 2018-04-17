module Phone where

import           Data.Char
import           Data.List
import           Data.Maybe

type Digit = Char

data Button = Button
  { digit  :: Digit
  , string :: String
  } deriving (Show)

newtype Phone = Phone
  { buttons :: [Button]
  } deriving (Show)

phone :: Phone
phone =
  Phone
    [ Button '1' "1"
    , Button '2' "abc2"
    , Button '3' "def3"
    , Button '4' "ghi4"
    , Button '5' "jkl5"
    , Button '6' "mno6"
    , Button '7' "pqrs7"
    , Button '8' "tuv8"
    , Button '9' "wxyz9"
    , Button '*' ""
    , Button '0' " 0"
    , Button '#' "."
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"
  ]

{-
containsCharacter 'A' (Button '2' "abc2") == True
-}
containsCharacter :: Char -> Button -> Bool
containsCharacter char (Button digit string) = toLower char `elem` string

-- Find the button for this letter
{-
filter (containsCharacter 'A') (buttons phone) == [Button '2' "abc2"]
-}
-- Find the number of presses required to generate this letter
{-
elemIndex 'a' "abc2" == Just 0
-}
--
type Presses = Int

-- Return the sequence of button presses needed to output the specified char
{-
reverseTaps phone 'a' == [('2', 1)]             -- i.e. tap 2 once
reverseTaps phone 'A' == [('*', 1), ('2', 1)]   -- i.e. tap * then 2
-}
reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps phone char
  | isAsciiLower char = [(digit', presses)]
  | otherwise = ('*', 1) : [(digit', presses)]
  where
    button = head $ filter (containsCharacter char) (buttons phone)
    digit' = digit button
    presses = 1 + fromJust (elemIndex (toLower char) (string button))

cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

-- TODO: Handle numeral input!
