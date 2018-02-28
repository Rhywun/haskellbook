type M = Maybe

{-
(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: M (a -> b) -> M a -> M b

pure :: a -> f a
pure :: a -> M a
-}

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if length s > maxLen
                          then Nothing
                          else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
{-
mkPerson n a = case mkName n of
                 Nothing -> Nothing
                 Just n' -> case mkAddress a of
                              Nothing -> Nothing
                              Just a' -> Just $ Person n' a'
-}
mkPerson n a = Person <$> mkName n <*> mkAddress a

-- Break it down

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

