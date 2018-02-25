-- How does (fmap . fmap) even typecheck?

fmap . fmap :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)

(.) :: (b -> c) -> (a -> b) -> a -> c

fmap :: Functor f => (m -> n) -> f m -> f n
fmap :: Functor g => (x -> y) -> f x -> f y

fmap . fmap :: (Functor f, Functor g) =>
  ((m -> n) -> f m -> f n) -> ((x -> y) -> f x -> f y) -> ??? -> ???

-- Completely lost here
