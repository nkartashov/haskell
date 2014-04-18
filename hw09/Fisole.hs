-- (3 балла)

-- Список экспорта менять нельзя!
module Fisole
    ( Fisole, runFisole
    , abortF, putCharF, getCharF
    ) where

import System.IO 
import System.IO.Error

data Fisole a = Fisole (Handle -> IO (Either String a))

-- Второй аргумент - имя файла с которым будут работать функции putCharF и getCharF.
-- Если произошли какие-то ошибки ввода-вывода (т.е. исключения бросались), нужно их поймать и вернуть Left с каким-нибудь сообщением об ошибке.
runFisole :: Fisole a -> String -> IO (Either String a)
runFisole (Fisole f) path = catchIOError (withFile path ReadWriteMode f) carry

carry :: IOError -> IO (Either String a)
carry = return . Left . ioeGetErrorString


instance Functor Fisole where
    fmap f (Fisole g) = Fisole (\h -> do
		r <- g h
		return $ fmap f r)

instance Monad Fisole where
	return v = Fisole (\_ -> return $ Right v)
	Fisole f1 >>= f2 = Fisole (\h -> do
		res <- f1 h
		case res of
			Left s -> return $ Left s
			Right v -> do
				let (Fisole f') = f2 v
				f' h)

-- abortF s завершает вычисление, s - сообщение об ошибке.
abortF :: String -> Fisole a
abortF s = Fisole (\_ -> return $ Left s)

putCharF :: Char -> Fisole ()
putCharF ch = Fisole (\h -> fmap Right $ hPutChar h ch)

getCharF :: Fisole Char
getCharF = Fisole (\h -> fmap Right $ hGetChar h)
