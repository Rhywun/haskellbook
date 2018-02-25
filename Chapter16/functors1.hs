data FixMePls = FixMe | Pls deriving (Eq, Show)

instance Functor FixMePls where
  fmap = error "it doesn't matter, it won't compile"

{-
functors1.hs:3:18: error:
    * Expected kind `* -> *', but `FixMePls' has kind `*'
    * In the first argument of `Functor', namely `FixMePls'
      In the instance declaration for `Functor FixMePls'
  |
3 | instance Functor FixMePls where
  |                  ^^^^^^^^
Failed, no modules loaded.
-}
