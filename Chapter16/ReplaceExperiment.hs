module Chapter16.ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap replaceWithP :: Functor f => f b -> f Char

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- Just making the argument `b` more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- fmap replaceWithP' :: Functor f => f [Maybe [Char]] -> f Char

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

-- Again we can assert a more specific type
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- fmap liftedReplace         :: (Functor f2, Functor f1) => f1 (f2 a) -> f1 (f2 Char)
-- (fmap . fmap) replaceWithP :: (Functor f2, Functor f1) => f1 (f2 a) -> f1 (f2 Char)

-- fmap liftedReplace' :: Functor f => f [Maybe [Char]] -> f [Char]
-- (fmap . fmap) replaceWithP'
--    :: (Functor f2, Functor f1) => f1 (f2 [Maybe [Char]]) -> f1 (f2 Char)

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- More specific or "concrete"
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

--

main :: IO ()
main = do
  putStr "replaceWithP' lms: "
  print (replaceWithP' lms)
  putStr "liftedReplace lms: "
  print (liftedReplace lms)
  putStr "liftedReplace' lms: "
  print (liftedReplace' lms)
  putStr "twiceLifted lms: "
  print (twiceLifted lms)
  putStr "twiceLifted' lms: "
  print (twiceLifted' lms)
  putStr "thriceLifted lms: "
  print (thriceLifted lms)
  putStr "thriceLifted' lms: "
  print (thriceLifted' lms)