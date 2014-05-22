

type Kleisli m a b = a -> m b

(>>>) :: Monad m => Kleisli m a b -> Kleisli m b c -> Kleisli m a c
f >>> g = \a -> do
	b <- f a
	g b

arrow :: Monad m => (a -> b) -> Kleisli m a b
arrow f = return . f

printFile = readFile >>> print


	
