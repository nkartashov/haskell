import Control.Monad

data Tree a = Leaf | Node a (Tree a) (Tree a)

tagTree :: Tree a -> Tree (a, Int)
tagTree t = go t ()
	where
		go :: Tree a -> State Int (Tree a, Int)
		go Leaf = get >> return (Leaf, 0)
		go (Node i l r) = do
			let ls = go l
			l' <- get ls
			ls	
