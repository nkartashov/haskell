module Tree where

-- (2 балла)

data Tree a = Node { value :: a, children :: [Tree a] }

instance Eq a => Eq (Tree a) where
	(Node a []) == (Node b []) =  a == b
	(Node a xa) == (Node b xb) = (a == b) && (xa == xb)
	

instance Read a => Read (Tree a) where
	readsPrec _ s = concatMap (\(value, s) -> parseRest value s) (reads s)
		where
			parseRest :: Read a => a -> String -> [(Tree a, String)]
			parseRest value (':':s) = case parseList s [] of
																 ((list, rest) : _) -> [(Node value list, rest)]
																 _ -> [(Node value [], ':' : s)]
			parseRest value s = [(Node value [], s)]

			parseList :: Read a => String -> [Tree a] -> [([Tree a], String)]
			parseList ('}' : xs) parsed = [(parsed, xs)]
			parseList (',' : xs) parsed = parseList ('{' : xs) parsed
			parseList ('{' : xs) parsed = case reads xs of
																		((tree, rest) : _) -> parseList rest (parsed ++ [tree])
																		_ -> []
			parseList _ _ = []


-- read "1:[2:[4:[],5:[]],3:[]]"


instance Show a => Show (Tree a) where
	show (Node a []) = show a
	show (Node a xa) = show a ++ ":{" ++ showChildren xa ++ "}"
		where 
			showChildren (x:[]) = show x
			showChildren (x:xs) = show x ++ "," ++ (showChildren xs)

instance Functor Tree where
	fmap f (Node a xa) = Node (f a) $ map (fmap f) xa
