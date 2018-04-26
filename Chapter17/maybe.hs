import           Control.Applicative

type M = Maybe

{-
(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: M (a -> b) -> M a -> M b

pure :: a -> f a
pure :: a -> M a
-}
--
{-
validateLength 6 "hello" == Just "hello"
validateLength 4 "hello" == Nothing
-}
validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
    then Nothing
    else Just s

newtype Name =
  Name String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

{-
mkName "Joe" == Just (Name "Joe")
mkName "Joeeeeeeeeeeeeeeeeeeeeeeeeeeee" == Nothing
-}
mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s -- remember, Name is a function!

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a

data Person =
  Person Name
         Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
--
{-
-- This won't work:
mkPerson n a = case mkName n of
                 Nothing -> Nothing
                 Just n' -> case mkAddress a of
                              Nothing -> Nothing
                              Just a' -> Just $ Person n' a'
-}
--
mkPerson n a = Person <$> mkName n <*> mkAddress a --
--
-- Break it down
--
{-
instance Functor Maybe  where
  fmap _ Nothing  = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure              = Just
  Nothing <*> _     = Nothing
  _ <*> Nothing     = Nothing
  Just f <*> Just a = Just (f a)
-}
--
-- see text
--
-- Before we moooove on
--
-- p. 1098