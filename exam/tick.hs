import Control.Monad.State

tick :: State Int Int
tick = do
  n <- get
  put $ n + 1
  return n

succ' :: Int -> Int
succ' = evalState tick

plus' :: Int -> Int -> Int
plus' n= execState $ replicateM n tick
