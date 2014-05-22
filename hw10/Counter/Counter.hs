-- (0.5 балла)
module Counter
    ( Counter
    , tick
    , runCounter
    ) where

-- Монада Counter считает количество тиков, т.е. вызовов функции tick
data Counter a = Counter Int a

-- Возвращает результат вычислений и количество тиков
runCounter :: Counter a -> (a, Int)
runCounter (Counter i v)  = (v, i)

instance Monad Counter where
	return v = Counter 0 v 
	(Counter i v) >>= g = do
		let (Counter i' v') = g v
		Counter (i + i') v'

tick :: Counter ()
tick = Counter 1 ()
