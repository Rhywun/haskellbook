module Main where

--
-- 13.10 - Step One: Importing modules
--
import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.List     (intersperse, sort)
import           Data.Maybe    (isJust)
import           System.Exit   (exitSuccess)
import           System.IO
import           System.Random (randomRIO)

--
-- 13.11 - Step Two: Generating a word list
--
-- type WordList = [String]
newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

-- | Filter allWords by specified length min and max
gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
      in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

-- | Give us a random word from gameWords
randomWord' :: IO String
randomWord' = gameWords >>= randomWord

--
-- 13.12 - Step Three: Making a puzzle
--
data Puzzle =
  Puzzle String -- ^ The word we're trying to guess
         [Maybe Char] -- ^ The characters we've filled in so far
         String -- ^ Or [Char], the list of letters we've guessed so far

-- | Render Puzzle including charaters filled in plus characters guessed
instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++
    " // Guessed so far: " ++ (intersperse ' ' $ sort guessed)

-- | Construct a list of Nothing, one for each char in the puzzle word
{-
freshPuzzle "hello" --> _ _ _ _ _ // Guessed so far:
-}
freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []
    -- Was:     Puzzle s [Nothing | _ <- s] []

-- | Whether the char is part of the puzzle word
{-
charInWord (freshPuzzle "hello") 'h' == True
-}
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = c `elem` w

-- | Whether the char has been guessed already
{-
alreadyGuessed (freshPuzzle "hello") 'h' == False
-}
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = c `elem` g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

-- | Insert correctly guessed character into the puzzle's list of discovered chars
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
    then do
      putStrLn "You win!"
      exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle =
  forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    hSetBuffering stdout NoBuffering
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
