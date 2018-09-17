module Main where

import           Control.Monad                  ( forever
                                                , when
                                                )
import           Morse                          ( morseToChar
                                                , stringToMorse
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO                      ( getLine
                                                , isEOF
                                                )

convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess
  -- Otherwise, proceed
  line <- getLine
  convertLine line
 where
  convertLine line = do
    let morse = stringToMorse line
    case morse of
      (Just str) -> putStrLn (unwords str)
      Nothing    -> do
        putStrLn $ "ERROR: " ++ line
        exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess
  -- Otherwise, proceed
  line <- getLine
  convertLine line
 where
  convertLine line = do
    let decoded :: Maybe String
        decoded = traverse morseToChar (words line)
    case decoded of
      (Just s) -> putStrLn s
      Nothing  -> do
        putStrLn $ "ERROR: " ++ line
        exitFailure

--

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] -> case arg of
      "from" -> convertFromMorse
      "to"   -> convertToMorse
      _      -> argError
    _ -> argError
 where
  argError = do
    putStrLn "Usage: morse [from|to]"
    exitFailure
