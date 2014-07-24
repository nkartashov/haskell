module BFSource (BFSource(BFSource),
                           BFCommand(GoRight, 
                                     GoLeft, 
                                     Increment, 
                                     Decrement, 
                                     Print, 
                                     Read, 
                                     LoopL, 
                                     LoopR),
                                     parseBF) where

import Data.Maybe as DM

data BFCommand = GoRight      -- >
                      | GoLeft       -- <
                      | Increment    -- +
                      | Decrement    -- -
                      | Print        -- .
                      | Read         -- ,
                      | LoopL        -- [
                      | LoopR        -- ]
                      | Comment Char -- anything else

data BFSource = BFSource [BFCommand]

instance Show BFSource where
  show (BFSource source) = map bfToChar source

charToBF :: Char -> BFCommand
charToBF '>' = GoRight
charToBF '<' = GoLeft
charToBF '+' = Increment
charToBF '-' = Decrement
charToBF '.' = Print
charToBF ',' = Read
charToBF '[' = LoopL
charToBF ']' = LoopR
charToBF  c  = Comment c

bfToChar :: BFCommand -> Char
bfToChar GoRight = '>'
bfToChar GoLeft = '<'
bfToChar Increment = '+'
bfToChar Decrement = '-'
bfToChar Print = '.'
bfToChar Read = ','
bfToChar LoopL = '['
bfToChar LoopR = ']'

charToBFClean :: Char -> Maybe BFCommand
charToBFClean c = case charToBF c of
  Comment _ -> Nothing
  v -> Just v

data BFSyntaxCheck = OKSyntax [Int]
                      | UnopenedParen Int

instance Show BFSyntaxCheck where
  show (OKSyntax (x:xa)) = "Unclosed paren at " ++ show x
  show (UnopenedParen i) = "Unopened paren at " ++ show i

start :: BFSyntaxCheck
start = OKSyntax []

accept :: BFSyntaxCheck -> (BFCommand, Int) -> BFSyntaxCheck
accept (OKSyntax xs) (LoopL, i) = OKSyntax $ i:xs
accept (OKSyntax (x:xs)) (LoopR, _) = OKSyntax xs
accept (OKSyntax []) (LoopR, i) = UnopenedParen i
accept c _ = c

finalize :: BFSource -> BFSyntaxCheck -> Either String BFSource
finalize source (OKSyntax []) = Right source
finalize _ check = Left $ show check

checkBFSyntax :: BFSource -> Either String BFSource
checkBFSyntax s@(BFSource source) = finalize s
                                  $ foldl accept start $ zip source [0..]
parseBF :: String -> Either String BFSource
parseBF = checkBFSyntax . BFSource . DM.mapMaybe charToBFClean
