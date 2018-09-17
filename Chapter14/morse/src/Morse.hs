module Morse
  ( Morse
  , charToMorse
  , morseToChar
  , stringToMorse
  , morseMap
  -- , morseToLetter
  )
where

import qualified Data.Map                      as M

type Morse = String

-- | A transliteration table of letters to Morse codes
morseMap :: (M.Map Char Morse)
morseMap = M.fromList
  [ ('a', ".-")
  , ('b', "-...")
  , ('c', "-.-.")
  , ('d', "-..")
  , ('e', ".")
  , ('f', "..-.")
  , ('g', "--.")
  , ('h', "....")
  , ('i', "..")
  , ('j', ".---")
  , ('k', "-.-")
  , ('l', ".-..")
  , ('m', "--")
  , ('n', "-.")
  , ('o', "---")
  , ('p', ".--.")
  , ('q', "--.-")
  , ('r', ".-.")
  , ('s', "...")
  , ('t', "-")
  , ('u', "..-")
  , ('v', "...-")
  , ('w', ".--")
  , ('x', "-..-")
  , ('y', "-.--")
  , ('z', "--..")
  , ('1', ".----")
  , ('2', "..---")
  , ('3', "...--")
  , ('4', "....-")
  , ('5', ".....")
  , ('6', "-....")
  , ('7', "--...")
  , ('8', "---..")
  , ('9', "----.")
  , ('0', "-----")
  ]

letterMap :: M.Map Morse Char
letterMap = M.foldrWithKey (flip M.insert) M.empty morseMap

{-
charToMorse 'a' -- Just ".-"
-}
charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c morseMap

{-
stringToMorse "sos" -- Just ["...","---","..."]
-}
stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse
    -- Was: stringToMorse s = sequence $ fmap charToMorse s

{-
morseToChar "." -- Just 'e'
-}
morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m letterMap
