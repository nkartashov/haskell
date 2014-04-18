import System.Environment (getArgs)
import Control.Exception (SomeException, catch)

handler :: SomeException -> IO ()
handler e = do
	putStrLn $ "Cannot locate file " ++ show e


handleFile :: String -> IO ()
handleFile path = do
	text <- readFile path
	putStrLn text

handledFile path = catch (handleFile path) handler

main :: IO ()
main = do
	args <- getArgs
	case args of
		[] -> interact id
		_ -> mapM_ handledFile args
