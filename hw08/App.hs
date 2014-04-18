-- (2 балла)

import Control.Applicative

data Tree a = Node a [Tree a] deriving (Show)

instance Functor Tree where
	fmap f (Node a ch) = Node (f a) $ map (fmap f) ch

instance Applicative Tree where
	pure a = Node a $ repeat $ pure a
	(Node f lf) <*> (Node v lv) = Node (f v) $ zipWith (<*>) lf lv
