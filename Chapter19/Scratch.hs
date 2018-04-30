module Scratch where

import           Control.Applicative
import           Data.Time.Clock

{-
offsetCurrentTime 10 == 2018-05-10 11:22:29.195835 UTC    -- 10 days later
-}
offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset = addUTCTime (offset * 24 * 3600) <$> getCurrentTime

--
f 9001 = True
f _    = False

g 42 = True
g _  = False

{-
(f <||> g) 0    == False
(f <||> g) 42   == True
(f <||> g) 9001 == True
-}
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)
