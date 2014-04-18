import Data.List (isInfixOf)
import System.Environment (getArgs)
import Control.Exception (SomeException, catch)

handler :: SomeException -> IO ()
handler e = putStrLn $ "Cannot locate file " ++ show e

handleFile :: String -> String -> IO ()
handleFile pattern path = do
	text <- readFile path
	let lns = filter (isInfixOf pattern) $ lines text
	mapM_ putStrLn lns
	
handledFile :: String -> String -> IO ()
handledFile pattern path = catch (handleFile pattern path) handler

main :: IO ()
main = do
	args <- getArgs
	case args of
		[] -> return ()
		(a:xs) -> mapM_ (handledFile a) xs
