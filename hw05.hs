import Prelude hiding(lookup)
import Test.HUnit
import Data.Maybe (isNothing)
import qualified Data.List as DL (sort, group)

-- 1. fun четные числа в нечетных позициях (нумеруя с 0) умножает на 2, остальные не изменяет.
--    (0.5 балла)
fun :: [Integer] -> [Integer]
fun = map f . zip [0..]
	where f (n, e)
				|	(n `mod` 2 == 1) && (e `mod` 2 == 0) = e * 2
				| otherwise														 = e

-- 2. Бесконечный список Фибоначчи.
--    (0.5 балла)
fibs :: [Integer]
fibs = map snd $ iterate (\(a, b) -> (b, a + b)) (0, 1)

-- 3a. shiftL переставляет первый элемент в конец списка. Реализуйте эту функцию так, чтобы она проходила по списку только один раз.
--     (0.5 балла)
shiftL :: [a] -> [a]
shiftL [] = []
shiftL [x] = [x]
shiftL (x:xs) = xs ++ [x]

-- 3b. shiftR переставляет последний элемент в начало. Реализуйте эту функцию так, чтобы она проходила по списку только один раз.
--     (0.5 балла)
shiftR :: [a] -> [a]
shiftR [] = []
shiftR [x] = [x]
shiftR xs = (\x -> (fst x) : (snd x)) (initLast xs)
	where
		initLast :: [a] -> (a, [a])
		initLast (x:[]) = (x, []) 
		initLast (x:xs) = (\y -> (fst (initLast y), x : snd (initLast y))) xs

-- 4. takeLast n xs возвращает последние n элементов списка xs.
--    (0.5 балла)
takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

-- 5. swap i j меняет местами i и j элементы.
--    (1 балл)
swap :: Int -> Int -> [a] -> [a]
swap i j xs
	| i >= l || j >= l = xs
	| otherwise        = map f $ zip [0..] xs
	where
		l = length xs
		f (n, e)
			| n == i = v2
			| n == j = v1
			| otherwise = e
				where
					v1 = xs !! i
					v2 = xs !! j

-- 6. Назовем элементы, которые удовлетворяют предикату p хорошими, остальные плохими.
--    Тогда mapl p f xs выбрасывает плохие элементы, а блоки подряд идущих хороших элементов,
--    которые разделяются плохими, отправляет в функцию f и возвращает список результатов.
--    Заметьте, что в функцию f никогда не передаются пустые списки.
--    (1 балл)
mapl :: (a -> Bool) -> ([a] -> b) -> [a] -> [b]
mapl p f xs = go p f $ dwnp xs
	where
		go p f [] = []
		go p f xs = (handle xs) : (go p f $ remove xs)
		handle = f . twp
		remove = dwnp . dwp
		twp = takeWhile p
		dwp	= dropWhile p
		dwnp = dropWhile np
			where
				np = not . p
						

------------------------------------------------------------------------------
-- 7.

data Tree a = Node a [Tree a]

elements :: Tree a -> [a]
elements (Node n []) = [n]
elements (Node n xn) = n : foldr1 (++) (map elements xn)


-- (a) Возвращает высоту дерева.
--     (1 балл)
height :: Tree a -> Int
height (Node _ []) = 1
height (Node _ xn) = maximum $ map (succ . height) xn

-- (b) Возвращает среднее арифметическое значений во всех узлах дерева.
--     Необходимо вычислить эту функцию, выполнив один проход по дереву.
--     (1 балл)
avg :: Tree Int -> Int
avg = average . elements
	where 
		average :: [Int] -> Int
		average xs = foldr1 (+) xs `div` length xs 

-- (c) Возвращает ширину дерева.
--     Ширина дерева определяется следующим образом:
--     Количество вершин на определенном уровне называется шириной уровня.
--     Ширина дерева - это максимальная ширина уровня по всем уровням.
--     (1 балл)
width :: Tree a -> Int
width =  maximum . tranformWidth . DL.group . DL.sort . levels 0
	where 
		tranformWidth = map length
		levels :: Int -> Tree a -> [Int]
		levels level (Node _ xn) = level : foldr (++) [] (map nextLevels xn)
			where
				nextLevels :: Tree a -> [Int]
				nextLevels = levels $ succ level
-- tests

(tree1, tree2, tree3) =
    ( b [b [l [b []],
            l [b [],
               l [b [l [],
                     l [],
                     b []],
                  l []]]],
         b [],
         b [],
         l []]
    , b [b [b [],
            b [b [],
               b []]],
         b [b [],
            l [b [],
               b []]],
         l [b []]]
    , b [tree1, tree2]
    )
  where l = Node 500; b = Node 300

(testsHeight, testsAvg, testsWidth) = (
    [ height tree1 ~?= 6
    , height tree2 ~?= 4
    , height tree3 ~?= 7
    ],
    [ avg tree1 ~?= 393
    , avg tree2 ~?= 330
    , avg tree3 ~?= 362
    ],
    [ width tree1 ~?= 4
    , width tree2 ~?= 5
    , width tree3 ~?= 7
    ])

------------------------------------------------------------------------------
-- 8. Реализовать двоичное дерево поиска без балансировки.
--    (4 балла)

data Map k v = LeafNode | MapNode k v ((Map k v), (Map k v)) deriving (Show)

consMap :: k -> v -> Map k v
consMap k v = MapNode k v (LeafNode, LeafNode)

mkey :: Map k v -> k
mkey (MapNode k v _) = k

mval :: Map k v -> v
mval (MapNode k v _) = v

leftCh :: Map k v -> Map k v
leftCh (MapNode _ _ (l, _)) = l

rightCh :: Map k v -> Map k v
rightCh (MapNode _ _ (_, r)) = r

isLeaf :: Map k v -> Bool
isLeaf LeafNode = True
isLeaf _ = False

hasChildren :: Map k v -> Bool
hasChildren = (\m -> (not . isLeaf $ leftCh m) && (not . isLeaf $ rightCh m))

-- Первый аргумент - функция, сравнивающая значения типа k.
-- Она вовзращает True, если первое значение меньше второго, иначе False.
lookup :: (k -> k -> Bool) -> k -> Map k v -> Maybe v
lookup comp key LeafNode = Nothing
lookup comp key m
	| eql key mk = Just mv
	|	comp mk key = lookup comp key $ leftCh m
	| otherwise = lookup comp key $ rightCh m
		where 
					eql k1 k2 = not $ (comp k1 k2) && (comp k2 k1)
					mk = mkey m
					mv = mval m

insert :: (k -> k -> Bool) -> k -> v -> Map k v -> Map k v
insert comp key value dest = insertNode comp dest $ consMap key value

insertFromList :: (k -> k -> Bool) -> Map k v -> [(k, v)] -> Map k v
insertFromList comp dest [] = dest
insertFromList comp dest ((key, value): xs) = insertFromList comp (insert comp key value dest) xs

insertNode :: (k -> k -> Bool) -> Map k v -> Map k v -> Map k v
insertNode comp dest elem
	|	eql key mk = dest
	|	comp mk key = insertLeft dest elem
	| otherwise = insertRight dest elem
		where
					eql k1 k2 = (not $ comp k1 k2) && (not $ comp k2 k1)
					key = mkey dest
					value = mval dest
					mk = mkey elem
					mv = mval elem
					insertLeft (MapNode kk vv (LeafNode, r)) elem = MapNode kk vv (elem, r)
					insertLeft (MapNode kk vv (l, r)) elem = MapNode kk vv (insertNode comp l elem, r)

					insertRight (MapNode kk vv (l, LeafNode)) elem = MapNode kk vv (l, elem)
					insertRight (MapNode kk vv (l, r)) elem = MapNode kk vv (l, insertNode comp r elem)
					

delete :: (k -> k -> Bool) -> k -> Map k v -> Maybe (Map k v)
delete comp mk dest
	| not $ hasChildren dest = Nothing
	|	otherwise = Just (deleteNode mk dest)
		where
					eql k1 k2 = (not $ comp k1 k2) && (not $ comp k2 k1)
					key = mkey dest
					value = mval dest
					deleteNode mk dest = insertFromList comp dest ((toList $ leftCh dest) ++ (toList $ rightCh dest)) 


fromList :: (k -> k -> Bool) -> [(k,v)] -> Map k v
fromList comp ((key, value):xs) = insertFromList comp (consMap key value) xs


toList :: Map k v -> [(k, v)]
toList = traverse
	where 
		traverse ::	Map k v -> [(k, v)]
		traverse LeafNode = []
		traverse (MapNode key value (l, r)) = (traverse l) ++ [(key, value)] ++ traverse r

-- tests

sort :: (a -> a -> Bool) -> [a] -> [a]
sort p = map fst . toList . fromList p . map (\x -> (x, ()))

------------------------------------------------------------------------------
-- main

main = fmap (\_ -> ()) $ runTestTT $ test
    $    label "fun"
    [ fun [1,3,6,10,15,21,28,30,60] ~?= [1,3,6,20,15,21,28,60,60]
    , take 11 (fun fibs) ~?= [1,1,2,3,5,16,13,21,34,55,89]
    ] ++ label "fibs"
    [ take 10 fibs ~?= [1,1,2,3,5,8,13,21,34,55]
    , fibs !! 1000 ~?= 70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501
    ] ++ label "shiftL"
    [ shiftL [1..20] ~?= [2..20] ++ [1]
    , shiftL [] ~?= ([] :: [Bool])
    ] ++ label "shiftR"
    [ shiftR [1..20] ~?= 20:[1..19]
    , shiftR [] ~?= ([] :: [Bool])
    ] ++ label "takeLast"
    [ takeLast 5 [1..20] ~?= [16,17,18,19,20]
    , takeLast 5 [1,2,3] ~?= [1,2,3]
    ] ++ label "swap"
    [ swap 1 2 [3,4,5,6] ~?= [3,5,4,6]
    , swap 2 0 "abcd" ~?= "cbad"
    , swap 100 7 [1..10] ~?= [1..10]
    ] ++ label "mapl"
    [ mapl (\x -> x `mod` 7 /= 3) id [1..20] ~?= [[1,2],[4,5,6,7,8,9],[11,12,13,14,15,16],[18,19,20]]
    , mapl (\x -> elem x [1,4,6,8,9,10,12,14,15,16,18,20]) sum [1..20] ~?= [1,4,6,27,12,45,18,20]
    ] ++ label "height" testsHeight
      ++ label "avg" testsAvg
      ++ label "width" testsWidth
      ++ label "Map" -- можете сами написать тесты на каждую функцию :)
    [ sort (<) [10,24,13,56,35,13,6,23] ~?= [6,10,13,23,24,35,56] ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
