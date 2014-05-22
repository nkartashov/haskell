-- (1 балл)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StateIO
    ( StateIO
    , runStateIO, execStateIO, evalStateIO
    ) where

import Control.Monad.State
import Data.IORef

newtype StateIO s a = StateIO { getStateIO :: IORef s -> IO a }

instance Monad (StateIO s) where
    return v = StateIO $ \s -> return v 
    (StateIO f) >>= f' = StateIO $ \s -> do
			res	<- f s
			(getStateIO $ f' res) s

instance MonadState s (StateIO s) where
    get = StateIO $ \s -> readIORef s
    put s = StateIO $ \_ -> return ()

runStateIO :: StateIO s a -> s -> IO (a,s)
runStateIO (StateIO f) v = do
	ref <- newIORef v
	r <- f ref
	st <- readIORef ref
	return (r, st)

execStateIO :: StateIO s a -> s -> IO s
execStateIO (StateIO f) v = do
	ref <- newIORef v
	_ <- f ref
	readIORef ref

evalStateIO :: StateIO s a -> s -> IO a
evalStateIO (StateIO f) v = do
	ref <- newIORef v
	f ref
