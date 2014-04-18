import Control.Concurrent (forkIO)
import Network (connectTo, PortID(..))
import System.Environment (getArgs)
import Control.Monad (forever)
import System.IO (hSetBuffering, BufferMode(..), hGetLine, hPutStrLn)
import GHC.IO.Handle (Handle)

listener :: Handle -> IO ()
listener h = forever $ do
	msg <- hGetLine h
	putStrLn msg

launch :: String -> PortID -> IO ()
launch host port = do
	h <- connectTo host port
	hSetBuffering h NoBuffering
	t <- forkIO $ listener h
	forever $ do
		line <- getLine
		hPutStrLn h line

displayPrompt :: IO ()
displayPrompt = putStrLn "Usage: telnet [host] [port]"

main :: IO ()
main = do
	args <- getArgs
	case args of
		(host : port : xs) -> launch host $ PortNumber $ fromIntegral $ (read port :: Integer)
		_ -> displayPrompt
