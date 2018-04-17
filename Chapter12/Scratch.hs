module Scratch where

--
-- 12.2 - How I learned to stop worrying and love Nothing
--
{-
ifEvenAdd2 2 == Just 4
ifEvenAdd2 3 == Nothing
-}
ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n =
  if even n
    then Just (n + 2)
    else Nothing

-- Smart constructors for datatypes
--
type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Show)

-- First attempt
{-
mkPerson1 "Jow Blow" 60 == Just (Person "Jow Blow" 60)
mkPerson1 "" 160 == Nothing
-}
mkPerson1 :: Name -> Age -> Maybe Person
mkPerson1 name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing

--
-- 12.3 - Bleating either
--
data PersonInvalid
  = NameEmpty
  | AgeTooLow
  deriving (Eq, Show)

mkPerson2 :: Name -> Age -> Either PersonInvalid Person
mkPerson2 name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow

--
type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age =
  case age >= 0 of
    True  -> Right age
    False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name =
  case name /= "" of
    True  -> Right name
    False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _             = Left badName
mkPerson' _ (Left badAge)              = Left badAge

--
-- 12.4 - Kinds, a thousand stars in your types
--
data Example a
  = Blah
  | RoofGoats
  | Woot a-- :k Example :: * -> *
-- :k Maybe :: * -> *
-- :k Maybe Int :: *
-- :k Either :: * -> * -> *
-- :k Either Int String :: *
