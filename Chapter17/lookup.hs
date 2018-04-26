import           Control.Applicative

{-
f 5 == Just "kbai"
-}
f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g x = lookup x [(7, "sup?"), (8, "chris"), (9, "aloha")]

{-
h 2 == Just 3
-}
h x = lookup x [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]

-- Maybe context
--
e1 = (++) <$> f 3 <*> g 7 -- -> Just "hellosup?"

e2 = liftA2 (++) (g 9) (f 4) -- -> Just "alohajulie"

e3 = liftA2 (^) (h 5) (m 4) -- -> Just 60466176

-- IO context
--
e4 = (++) <$> getLine <*> getLine -- length <$> e4

e5 = (,) <$> getLine <*> getLine
