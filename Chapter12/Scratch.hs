module Chapter12.Scratch where

--
-- 12.2 - How I learned to stop worrying and love Nothing
--

{-
ifEvenAdd2 2 == Just 4
ifEvenAdd2 3 == Nothing
-}
ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing

-- Smart constructors for datatypes

type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Eq, Show)

-- First attempt - we'll do better with `Either` below
{-
mkPerson1 "Joe Blow" 60 == Just (Person "Joe Blow" 60)
mkPerson1 "" 160        == Nothing
-}
mkPerson1 :: Name -> Age -> Maybe Person
mkPerson1 name age | name /= "" && age >= 0 = Just $ Person name age
                   | otherwise              = Nothing

--
-- 12.3 - Bleating either
--

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  deriving (Eq, Show)

{-
mkPerson2 "Joe Blow" 60 == Right (Person "Joe Blow" 60)
mkPerson2 "" 160        == Left NameEmpty
mkPerson2 "Joe" (-1)    == Left AgeTooLow
mkPerson2 "" (-1)       == Left NameEmpty      <-- only one error
-}
mkPerson2 :: Name -> Age -> Either PersonInvalid Person
mkPerson2 name age | name /= "" && age >= 0 = Right $ Person name age
                   | name == ""             = Left NameEmpty
                   | otherwise              = Left AgeTooLow

-- Now with better validation

type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = if age >= 0 then Right age else Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = if name /= "" then Right name else Left [NameEmpty]

{-
mkPerson "Joe Blow" 60 == Right (Person "Joe Blow" 60)
mkPerson "" 160        == Left [NameEmpty]
mkPerson "Joe" (-1)    == Left [AgeTooLow]
mkPerson "" (-1)       == Left [NameEmpty,AgeTooLow]    <-- both errors
-}
mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk ) (Right ageOk ) = Right (Person nameOk ageOk)
mkPerson' (Left  badName) (Left  badAge) = Left (badName ++ badAge)
mkPerson' (Left  badName) _              = Left badName
mkPerson' _               (Left badAge)  = Left badAge
    -- Later we'll simplify this with `liftA2`

--
-- 12.4 - Kinds, a thousand stars in your types
--

data Example a
  = Blah
  | RoofGoats
  | Woot a --
-- :k Example :: * -> *
-- :k Maybe :: * -> *
-- :k Maybe Int :: *
-- :k Either :: * -> * -> *
-- :k Either Int String :: *
-- :k Maybe Example              <-- Bzzzt
-- :k Maybe (Example Int) :: *

-- The `Just` data constructor is like a function:
js = fmap Just [1, 2, 3] -- [Just 1,Just 2,Just 3]
