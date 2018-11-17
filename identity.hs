data Id a = Id a
    deriving (Show, Eq)

--Id is a type function --> it sends every type in Hask to another one (it's an endofunctor)
--in a way that preserves function composition
instance Functor Id where
    fmap f (Id v) = Id (f v)

instance Applicative Id where
    f <*> v = fmap (fromId f) v
    pure = Id

instance Monad Id where
    idv >>= f = f . fromId $ idv
    return = Id

--the special thing is that fromId is _total_, so it succeeds every time, in the most natural way
--this, in general, is not the case
fromId :: Id t -> t
fromId (Id v) = v

--fmap :: Functor f => (a -> b) -> f a -> f b
--fmap' :: Functor f => (f a -> f b) -> a -> b

--the totality of fromId carries into the totality of the reverse fmap
--this, taken all together, shows explicitly that Hask and Id Hask are the same thing, modulo bottom
fmap' :: (Id a -> Id b) -> a -> b
fmap' f = fromId . f . Id 

tomonadic :: (a -> b) -> a -> Id b
tomonadic = fmap Id