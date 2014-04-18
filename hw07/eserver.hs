import System.Environment (getArgs)
import Network (listenOn, accept, PortID(..))
import GHC.IO.Handle (Handle)
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import System.IO (hGetLine, hPutStrLn)

displayPrompt :: IO ()
displayPrompt = putStrLn "Usage: eserver [port]"

startServer :: PortID -> IO ()
startServer port = do
	socket <- listenOn port
	forever $ do
		(h, _, _) <- accept socket
		forkIO $ echoer h

echoer :: Handle -> IO ()
echoer h = forever $ do
	line <- hGetLine h
	hPutStrLn h line

main :: IO ()
main = do
	args <- getArgs
	case args of
		(port : []) -> startServer $ PortNumber $ fromIntegral $ (read port :: Integer)
		_ -> displayPrompt
