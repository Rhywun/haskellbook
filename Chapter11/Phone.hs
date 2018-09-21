module Chapter11.Phone where

import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe

-- validButtons = "1234567890*#"
type Digit = Char

data Button = Button
  { digit  :: Digit
  , string :: String
  } deriving (Show)

newtype Phone = Phone
  { buttons :: [Button]
  } deriving (Show)

phone :: Phone
phone = Phone
  [ Button '1' "1"
  , Button '2' "abc2"
  , Button '3' "def3"
  , Button '4' "ghi4"
  , Button '5' "jkl5"
  , Button '6' "mno6"
  , Button '7' "pqrs7"
  , Button '8' "tuv8"
  , Button '9' "wxyz9"
  , Button '*' "^*"
  , Button '0' "+ 0"
  , Button '#' ".,#"
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
containsCharacter 'A' (Button '2' "abc2") -- True
-}
containsCharacter :: Char -> Button -> Bool
containsCharacter char (Button digit string) = toLower char `elem` string

-- Find the button for this letter

{-
filter (containsCharacter 'A') (buttons phone) -- [Button '2' "abc2"]
-}

-- Find the number of presses required to generate this letter
{-
elemIndex 'a' "abc2" -- Just 0
-}

-- Valid presses: 1 and up
type Presses = Int

-- Return the sequence of button presses needed to output the specified char
{-
reverseTaps phone 'a' -- [('2', 1)]             -- i.e. tap 2 once
reverseTaps phone 'A' -- [('*', 1), ('2', 1)]   -- i.e. tap * then 2
-}
reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps phone char | isAsciiLower char || isDigit char = [(digit', presses)]
                       | otherwise = ('*', 1) : [(digit', presses)]
 where
  button  = head $ filter (containsCharacter char) (buttons phone)
  digit'  = digit button
  presses = 1 + fromJust (elemIndex (toLower char) (string button))

{-
cellPhonesDead phone "Lol" -- [('*',1),('5',3),('6',3),('5',3)]
cellPhonesDead phone "123" -- [('1',1),('2',4),('3',4)]
-}
cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

messages = map (cellPhonesDead phone) convo

x |> f = f x -- found this on GitHub

{-
fingerTaps [('*',1),('5',3),('6',3),('5',3)] -- 10
fingerTaps [('1',1),('2',4),('3',4)]         -- 9
-}
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps taps = map snd taps |> sum -- or: sum $ map snd taps

allFingerTaps = map fingerTaps messages

-- N.B. - Ignoring punctation, spaces, and capitalization here

{-
mostPopularLetter (convo !! 0)  -- 'n'
mostPopularLetter "Mississippi" -- 's'
-}
mostPopularLetter :: String -> Char
mostPopularLetter s = sort s & group & sortOn length & reverse & head & head

{-
coolestLtr convo -- ' '
-}
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

{-
coolestWord convo -- "Lol"
-}
coolestWord :: [String] -> String
coolestWord ss =
  unwords ss & words & sort & group & sortOn length & reverse & head & head
