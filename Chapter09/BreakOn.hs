module Chapter09.BreakOn where

{-
-- Generalize these:

myWords :: String -> [String]
myWords ""      = []
myWords (' ':s) = myWords s
myWords s       = w : myWords t
    where w = takeWhile (/=' ') s
          t = dropWhile (/=' ') s

myLines :: String -> [String]
myLines ""       = []
myLines ('\n':s) = myLines s
myLines s        = w : myLines t
    where w = takeWhile (/='\n') s
          t = dropWhile (/='\n') s
-}

breakOn :: Char -> String -> [String]
breakOn _ ""  = []
breakOn c s | c == head s = breakOn c (tail s)
            | otherwise   = w : breakOn c t
    where w = takeWhile (/=c) s
          t = dropWhile (/=c) s

-- Test

s1 = "sheryl wants fun"

s21 = "Tyger Tyger, burning bright\n"
s22 = "In the forests of the night\n"
s23 = "What immortal hand or eye\n"
s24 = "Could frame thy fearful symmetry?"
s2 = s21 ++ s22 ++ s23 ++ s24

-- myWords s1 == ["sheryl","wants","fun"]
myWords = breakOn ' '

-- myLines s2 == ["Tyger Tyger, burning bright","In the forests of the night", ...]
myLines = breakOn '\n'
