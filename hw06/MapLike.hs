import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.List as L

-- (2 балла)

-- 1. Определить класс типов MapLike.
--    В нем должны быть функции empty, lookup, insert, delete, fromList с типами как в Data.Map.
--    Напишите реализацию по умолчанию для fromList.

class MapLike c where
	empty :: Ord k => c k v
	lookup :: Ord k => k -> c k v -> Maybe v
	insert :: Ord k => k -> v -> c k v -> c k v
	delete :: Ord k => k -> c k v -> c k v
	fromList :: Ord k => [(k, v)] -> c k v
	fromList = foldr (\(k, v) -> insert k v) empty

-- 2. Определить instance MapLike для (a) Data.Map, (b) ListMap и (c) ArrMap
--    Можно использовать любые стандартные функции.

newtype ListMap k v = ListMap [(k,v)]

newtype ArrMap k v = ArrMap (k -> Maybe v)

instance MapLike M.Map where
	empty = M.fromList []
	lookup k m = M.lookup
	insert = M.insert
	delete = M.delete
	fromList = M.fromList

instance MapLike ListMap where
	empty = ListMap []
	lookup k (ListMap []) = Nothing
	lookup k (ListMap ((key, value):xs)) = case k == key of
		True -> Just value
		_ -> lookup k $ ListMap xs
	insert key value (ListMap l) = ListMap $ map helper l
		where
			helper (k, v) = case key == k of
				True -> (k, value)
				_ -> (k, v)
	
	delete key (ListMap l) = ListMap $ filter (\(k, v) -> key == k) l
	fromList = ListMap

instance MapLike ArrMap where
	empty = ArrMap (\x -> Nothing)
	lookup k (ArrMap f) = f k
	insert k v (ArrMap f) = ArrMap (\key -> if key == k then Just v else f k)	
	delete k (ArrMap f) = ArrMap (\key -> if key == k then Nothing else f k)

-- 3. Написать instance Functor для (a) ListMap k и (b) ArrMap k.
instance Functor (ListMap k) where
	fmap f (ListMap l) = ListMap $ map (\(k, v) -> (k, f v)) l 

instance Functor (ArrMap k) where
	fmap f (ArrMap func) = ArrMap (\key -> helper key)
		where
			helper k = case func k of
				Nothing -> Nothing
				Just v	-> Just $ f v
