import  Control.Monad (forever)

-- Реализуйте runBehaviour
-- (1 балл)

data Request  = Get           | Put String
data Response = Result String | OK

type Behaviour = [Response] -> [Request]

prog :: Behaviour
prog ~(OK : x : xs) = Put "more? " : Get : case x of
    Result "no" -> []
    Result "yes" -> prog xs
    _ -> Put "yes or no!" : prog (tail xs)
 

runBehaviour :: Behaviour -> IO ()
runBehaviour b = shortCall 0 []
	where 
		shortCall = (\o r -> (progCall (succ o) r $ (drop o $ b r))) 
		progCall offset resp req = case req of
																 [] -> return ()
																 (Get : _) -> do
																				line <- getLine
																				shortCall offset (resp ++ [Result line]) 
																 (Put s : _) -> do
																				putStrLn s
																				shortCall offset (resp ++ [OK]) 


main :: IO ()
main = runBehaviour prog
