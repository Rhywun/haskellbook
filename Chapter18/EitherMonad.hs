module EitherMonad where

type Founded = Int -- years ago

type Coders = Int -- number of programmers

data SoftwareShop = Shop
  { founded     :: Founded
  , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError
  = NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded
                          Coders
  deriving (Eq, Show)

{-
validateFounded 5    == Right 5
validateFounded 5000 == Left (TooManyYears 5000)
-}
validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

{-
validateCoders 10    == Right 10
validateCoders 10000 == Left (TooManyCoders 10000)
-}
validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

{-
mkSoftware 50 5  == Right (Shop {founded = 50, programmers = 5})
mkSoftware 5 500 == Left (TooManyCodersForYears 5 500)
-}
mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > founded `div` 10
    then Left $ TooManyCodersForYears founded programmers
    else Right $ Shop founded programmers
