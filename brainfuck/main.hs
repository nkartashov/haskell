import System.IO (hFlush, stdout)
import Data.Char (ord, chr)
import System.Environment (getArgs)

import Tape
import BFSource

runBF :: BFSource -> IO ()
runBF (BFSource []) = return ()
runBF source = run emptyTape $ bfSource2Tape source

bfSource2Tape :: BFSource -> Tape BFCommand
bfSource2Tape (BFSource (b:bs)) = Tape [] b bs

type DataTape = Tape Int
type SourceTape = Tape BFCommand

run :: DataTape -> SourceTape -> IO ()
run dataTape source@(Tape _ GoRight _) = advance (moveRight dataTape) source
run dataTape source@(Tape _ GoLeft _) = advance (moveLeft dataTape) source
run (Tape l p r) source@(Tape _ Increment _) =
      advance (Tape l (p + 1) r) source
run (Tape l p r) source@(Tape _ Decrement _) =
      advance (Tape l (p - 1) r) source
run dataTape@(Tape _ p _) source@(Tape _ Print _) = do
      putChar (chr p)
      hFlush stdout
      advance dataTape source
run dataTape@(Tape l _ r) source@(Tape _ Read _) = do
      p <- getChar
      advance (Tape l (ord p) r) source
run dataTape@(Tape _ p _) source@(Tape _ LoopL _)
      | p == 0 = seekLoopR 0 dataTape source
      | otherwise = advance dataTape source
run dataTape@(Tape _ p _) source@(Tape _ LoopR _)
      | p /= 0 = seekLoopL 0 dataTape source
      | otherwise = advance dataTape source

run dataTape source = advance dataTape source

seekLoopR :: Int -> DataTape -> SourceTape -> IO ()
seekLoopR 1 dataTape source@(Tape _ LoopR _) = advance dataTape source
seekLoopR b dataTape source@(Tape _ LoopR _) =
      seekLoopR (b - 1) dataTape (moveRight source)
seekLoopR b dataTape source@(Tape _ LoopL _) =
      seekLoopR (b + 1) dataTape (moveRight source)
seekLoopR b dataTape source =
      seekLoopR b dataTape (moveRight source)

seekLoopL :: Int -> DataTape -> SourceTape -> IO ()
seekLoopL 1 dataTape source@(Tape _ LoopL _) = advance dataTape source
seekLoopL b dataTape source@(Tape _ LoopL _) =
      seekLoopL (b - 1) dataTape (moveLeft source)
seekLoopL b dataTape source@(Tape _ LoopR _) =
      seekLoopL (b + 1) dataTape (moveLeft source)
seekLoopL b dataTape source =
      seekLoopL b dataTape (moveLeft source)

advance :: DataTape -> SourceTape -> IO ()
advance dataTape (Tape _ _ []) = return ()
advance dataTape source = run dataTape (moveRight source)

main :: IO ()
main = do
  [filename] <- getArgs
  source <- readFile filename
  case parseBF source of
    Left s -> print s
    Right commands -> runBF commands
