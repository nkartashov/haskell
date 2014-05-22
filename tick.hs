
import Control.Monad.State

tick :: State Int Int
tick = do
	n <- get
	put $ n + 1
	return n

plus' n = execState $ replicateM n tick

fac' n = product $ evalState (replicateM n tick) 1
