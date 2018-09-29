{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Chapter24.Ini where

import           Control.Applicative
import           Data.ByteString                ( ByteString )
import           Data.Char                      ( isAlpha )
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO
import           Test.Hspec
import           Text.RawString.QQ
import           Text.Trifecta

-- Header parsing

headerEx :: ByteString
headerEx = "[blah]"

-- "[blah]" -> Section "blah"
newtype Header = Header String deriving (Eq, Ord, Show)

-- Discard the brackets, return the p result
parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

{-
           some letter :: CharParsing f => f [Char]
Header <$> some letter :: CharParsing f => f Header

parseString (some parseHeader) mempty "[blah]" -- Success [Header "blah"]
-}
parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

-- Assignment parsing

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

-- This allows parsing multiple lines
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

{-
parseString (some parseAssignment) mempty "key=value\n\n\ntest=data"
  -- Success [("key","value"),("test","data")]
-}
parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _    <- char '='
  val  <- some (noneOf "\n")
  skipEOL -- important!
  return (name, val)

-- Comment parsing

commentEx :: ByteString
commentEx = "; last modified 2001-04-01 by John Doe"

commentEx' :: ByteString
commentEx' = "; blah\n; woot\n  \n;hah"

{-
parseString (some skipComments) mempty "; comment"
-}
skipComments :: Parser ()
skipComments = skipMany
  (do
    _ <- char ';' <|> char '#'
    skipMany (noneOf "\n")
    skipEOL
  )

-- Section parsing

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

data Section = Section Header Assignments deriving (Eq, Show)

newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

{-
parseByteString parseSection mempty sectionEx
  -- Success (Section (Header "states") (fromList [("Chris","Texas")]))
parseByteString parseSection mempty sectionEx'
  -- Success (Section (Header "states") (fromList [("Chris","Texas")]))
parseByteString parseSection mempty sectionEx''
  -- Success (Section (Header "section") (fromList [("alias","claw"),("host","wikipedia.org")]))
-}
parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  as <- some parseAssignment
  return $ Section h (M.fromList as)

-- Ini file parsing

-- Roll up multiple sections into a Map
rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h as) = M.insert h as

{-
parseByteString parseIni mempty sectionEx
  -- Success (Config (fromList [(Header "states",fromList [("Chris","Texas")])]))
parseByteString parseIni mempty sectionEx''
  -- Success (Config (fromList [(Header "section",fromList [("alias","claw"),("host","wikipedia.org")]),
                                (Header "whatisit",fromList [("red","intoothandclaw")])]))
-}
parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return (Config mapOfSections)

-- Testing

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _           = Nothing

main :: IO ()
main = hspec $ do
  describe "Assignment Parsing" $ it "can parse a simple assignment" $ do
    let m  = parseByteString parseAssignment mempty assignmentEx
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just ("woot", "1")
  describe "Header Parsing" $ it "can parse a simple header" $ do
    let m  = parseByteString parseHeader mempty headerEx
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just (Header "blah")
  describe "Comment parsing" $ it "Skips comment before header" $ do
    let p  = skipComments >> parseHeader
        i  = "; woot\n[blah]"
        m  = parseByteString p mempty i
        r' = maybeSuccess m
    print m
    r' `shouldBe` Just (Header "blah")
  describe "Section parsing" $ it "can parse a simple section" $ do
    let m         = parseByteString parseSection mempty sectionEx
        r'        = maybeSuccess m
        states    = M.fromList [("Chris", "Texas")]
        expected' = Just (Section (Header "states") states)
    print m
    r' `shouldBe` expected'
  describe "INI parsing" $ it "Can parse multiple sections" $ do
    let
      m              = parseByteString parseIni mempty sectionEx''
      r'             = maybeSuccess m
      sectionValues  = M.fromList [("alias", "claw"), ("host", "wikipedia.org")]
      whatisitValues = M.fromList [("red", "intoothandclaw")]
      expected'      = Just
        (Config
          (M.fromList
            [(Header "section", sectionValues), (Header "whatisit", whatisitValues)]
          )
        )
    print m
    r' `shouldBe` expected'
