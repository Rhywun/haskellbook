data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor (FixMePls a) where
  fmap _ FixMe   = FixMe
  fmap f (Pls a) = Pls (f a)

{-
functors4.hs:3:19: error:
    * Expecting one fewer arguments to `FixMePls a'
      Expected kind `* -> *', but `FixMePls a' has kind `*'
    * In the first argument of `Functor', namely `(FixMePls a)'
      In the instance declaration for `Functor (FixMePls a)'
  |
3 | instance Functor (FixMePls a) where
  |                   ^^^^^^^^^^
Failed, no modules loaded.
-}