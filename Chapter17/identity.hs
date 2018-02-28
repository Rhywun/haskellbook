import Data.Functor.Identity

-- Identity gives us a bit of meaningless structure that we can lift over.

type Id = Identity

xs4  = [1, 2 ,3]
xs4' = [9, 9, 9]

i4 = const <$> xs4 <*> xs4'             -- [1,1,1,2,2,2,3,3,3]

mkId = Identity
j4 = const <$> mkId xs4 <*> mkId xs4'   -- [1,2,3]
