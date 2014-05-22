-- (4 балла)
import System.Random
import System.Environment
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent

{-
Решите задачу о философах http://en.wikipedia.org/wiki/Dining_philosophers_problem
Количество философов передается в параметрах командной строки.
Жизненый цикл философа:
a) Философ сообщает (вывод сообщения на экран) о том, что он готов обедать.
b) Ждет пока не освободятся обе вилки.
c) Берет вилки, сообщает об этом, начинает обедать, что занимает рандомное время (от 1 до 3 секунд).
d) Кладет вилки, сообщает об этом, начинает думать, что занимает рандомное время (от 1 до 3 секунд).
e) Возвращается к шагу (a).

Для реализации используйте библиотеку STM.
Вам также понадобятся функции forkIO, threadDelay и randomRIO.
-}

second :: Int -> Int
second = (* 1000000)

delay :: IO ()
delay = do
	time <- randomRIO (1, 3)
	threadDelay $ second time

getFork :: TVar Bool -> STM ()
getFork f = do
	status <- readTVar f
	case status of
		False -> writeTVar f True
		_ -> retry

freeFork :: TVar Bool -> STM ()
freeFork f = writeTVar f False

output :: Int -> String -> IO ()
output n s = putStrLn $ "Philosopher " ++ show n ++ s

philosopher :: (String -> IO ()) -> (Int, TVar Bool) -> (Int, TVar Bool) -> IO ()
philosopher out lf rf = forever $ do
	out " is ready"
	
	case (fst lf) < (fst rf)  of
		True -> do
			atomically $ getFork $ snd lf
			atomically $ getFork $ snd rf
		_ -> do
			atomically $ getFork $ snd rf
			atomically $ getFork $ snd lf
		
	out " started eating"
	delay
	out " finished eating"

	case (fst lf) < (fst rf)  of
		True -> do
			atomically $ freeFork $ snd rf
			atomically $ freeFork $ snd lf
		_ -> do
			atomically $ freeFork $ snd lf
			atomically $ freeFork $ snd rf

	out " rests"
	delay

main :: IO ()
main = do
	args <- getArgs
 	let phs = read $ head args :: Int
	forks <- replicateM phs $ newTVarIO False
	let numforks = zip [0..] forks 
	forM_ [0..(pred phs)] $ \n -> forkIO $ philosopher (output n) (numforks !! n) $ numforks !! ((succ n) `mod` phs)
	forever delay
