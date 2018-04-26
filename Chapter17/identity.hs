import           Data.Functor.Identity

-- Identity gives us a bit of meaningless structure that we can lift over.
--
{-
-- f ~ Identity
-- Applicative f =>
type Id = Identity

(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: Id (a -> b) -> Id a -> Id b

pure :: a -> f a
pure :: a -> Id a
-}
--
xs = [1, 2, 3]

xs' = [9, 9, 9]

r = const <$> xs <*> xs' -- [1,1,1,2,2,2,3,3,3]

mkId = Identity

r' = const <$> mkId xs <*> mkId xs' -- [1,2,3]
