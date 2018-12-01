module TryExcept where

import Control.Exception

{-
> willIFail 1
5
Right ()
> willIFail 0
Left divide by zero
-}
willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try $ print $ div 5 denom

