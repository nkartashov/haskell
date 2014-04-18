import qualified Data.List as L

main = do
	contents <- readFile "input"
	writeFile "output" $ unlines $ map unwords $ L.transpose $ map words $ lines $ contents
