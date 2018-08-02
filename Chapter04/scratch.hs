module Chapter04.Scratch where

--
-- Comparing values
--
data Mood
  = G
  | B
  deriving (Show)

--
-- Tuples
--
myTuple = (1 :: Integer, "blah")

fst' :: (a, b) -> a
fst' (a, b) = a

snd' :: (a, b) -> b
snd' (a, b) = b

-- tupFunc (2, "two") (3, "three") == (5,"twothree")
tupFunc :: (Int, [a]) -> (Int, [a]) -> (Int, [a])
tupFunc (a, b) (c, d) = (a + c, b ++ d)

-- call it: tupFunc (2, "two") (3, "three")
--
--
-- Lists
--
p = "Papuchon"

awesome = [p, "curry", ":)"]

s = "The Simons"

also = ["Quake", s]

allAwesome = [awesome, also]

--
--
type Name = String

data Pet
  = Cat
  | Dog Name
