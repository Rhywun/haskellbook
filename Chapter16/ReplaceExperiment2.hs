module ReplaceExperiment2 where

replaceWithP :: b -> Char
replaceWithP = const 'p'

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap replaceWithP :: Functor f => f b -> f Char
--
lms :: [Maybe (Integer, Integer)]
lms = [Just (1, 2), Nothing, Just (3, 4)]

-- Just making the argument `b` more specific
replaceWithP' :: [Maybe (Integer, Integer)] -> Char
replaceWithP' = replaceWithP

-- fmap replaceWithP' :: Functor f => f [Maybe (Integer, Integer)] -> f Char
liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- Again we can assert a more specific type
liftedReplace' :: [Maybe (Integer, Integer)] -> [Char]
liftedReplace' = liftedReplace

-- fmap liftedReplace         :: (Functor f2, Functor f1) => f1 (f2 a) -> f1 (f2 Char)
-- (fmap . fmap) replaceWithP :: (Functor f2, Functor f1) => f1 (f2 a) -> f1 (f2 Char)
--
-- fmap liftedReplace' :: Functor f => f [Maybe (Integer, Integer)] -> f [Char]
-- (fmap . fmap) replaceWithP'
--    :: (Functor f2, Functor f1) => f1 (f2 [Maybe (Integer, Integer)]) -> f1 (f2 Char)
--
twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe Integer] -> [Maybe Char]
twiceLifted' = twiceLifted

{-
thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- More specific or "concrete"
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted
--}
--
main :: IO ()
main = do
  putStr "replaceWithP' lms: "
  print (replaceWithP' lms)
  putStr "liftedReplace lms: "
  print (liftedReplace lms)
  putStr "liftedReplace' lms: "
  print (liftedReplace' lms)
  -- putStr "twiceLifted lms: "
  -- print (twiceLifted lms)
  -- putStr "twiceLifted' lms: "
  -- print (twiceLifted' lms)
  -- putStr "thriceLifted lms: "
  -- print (thriceLifted lms)
  -- putStr "thriceLifted' lms: "
  -- print (thriceLifted' lms)
