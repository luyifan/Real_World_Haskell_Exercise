-- file: FunctorLaw.hs
fmap id = id
fmap (f . g ) = fmap f . fmap g
