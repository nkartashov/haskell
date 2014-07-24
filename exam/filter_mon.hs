import Control.Monad


filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = do
  e <- xs
  case p e of
    True -> return e
    _ -> mzero
