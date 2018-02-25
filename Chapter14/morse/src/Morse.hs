module Morse
       ( Morse
       , charToMorse
       , morseToChar
       , stringToMorse
       , letterToMorse
       , morseToLetter
       )
       where

import qualified Data.Map as Map

type Morse = String

letterToMorse :: (Map.Map Char Morse)
letterToMorse = Map.fromList [
    ('a', ".-"),    ('b', "-..."),  ('c', "-.-."),  ('d', "-.."),   ('e', ".")
  , ('f', "..-."),  ('g', "--."),   ('h', "...."),  ('i', ".."),    ('j', ".---")
  , ('k', "-.-"),   ('l', ".-.."),  ('m', "--"),    ('n', "-."),    ('o', "---")
  , ('p', ".--."),  ('q', "--.-"),  ('r', ".-."),   ('s', "..."),   ('t', "-")
  , ('u', "..-"),   ('v', "...-"),  ('w', ".--"),   ('x', "-..-"),  ('y', "-.--")
  , ('z', "--.."),  ('1', ".----"), ('2', "..---"), ('3', "...--"), ('4', "....-")
  , ('5', "....."), ('6', "-...."), ('7', "--..."), ('8', "---.."), ('9', "----.")
  , ('0', "-----")
  ]

morseToLetter :: Map.Map Morse Char
morseToLetter = Map.foldrWithKey (flip Map.insert) Map.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = Map.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse s = sequence $ fmap charToMorse s

morseToChar :: Morse -> Maybe Char
morseToChar m = Map.lookup m morseToLetter

