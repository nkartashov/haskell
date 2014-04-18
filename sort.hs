import Data.List

sort' :: (Ord a) => [a] -> [a]
sort' [] = []
sort' (x : xs) = (sort' left) ++ [x] ++ (sort' right)
	where
		(left, right) = partition (<= x) xs
