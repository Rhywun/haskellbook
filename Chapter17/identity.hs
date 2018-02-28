import Data.Functor.Identity

-- Identity gives us a bit of meaningless structure that we can lift over.

type Id = Identity

xs  = [1, 2 ,3]
xs' = [9, 9, 9]

r = const <$> xs <*> xs'                -- [1,1,1,2,2,2,3,3,3]

mkId = Identity
r' = const <$> mkId xs <*> mkId xs'   -- [1,2,3]
