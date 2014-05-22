-- (0.5 балла)
import Counter
import Control.Monad

-- Эти две функции отличаются от обычных тем, что, вызывая tick, они считают сколько шагов заняло их выполнение.
filter' :: (a -> Bool) -> [a] -> Counter [a]
filter' p l = replicateM (length l) tick >>= \_ -> return $ filter p l

append :: [a] -> [a] -> Counter [a]
append l r = replicateM (length l) tick >>= \_ -> return $ l ++ r

-- Реализуйте qsort через filter' и append
qsort :: Ord a => [a] -> Counter [a]
qsort [] = return []
qsort (x:xs) = do
	l <- filter' (< x) xs >>= qsort
	r <- filter' (> x) xs >>= qsort
	res <- append l [x]
	append res r

-- Первый вызов должен занимать большее количество тиков ~ в 2 раза
main = do
    print $ runCounter $ qsort [1..15]
    print $ runCounter $ qsort [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15]
