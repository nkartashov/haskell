import System.Random (randomRIO)

handleInput :: Integer -> Integer -> IO ()
handleInput secret turn = do
	line <- getLine
	let number = read (head $ words line) :: Integer
	case number == secret of
		True -> putStrLn "You win"
		_ -> case turn < 5 of
			False -> putStrLn "You lose"
			_ -> do
				case compare number secret of
					LT -> putStrLn "Your number is less than secret"
					GT -> putStrLn "Yout number is greater than secret"
				handleInput secret $ succ turn


main :: IO ()
main = do
	secret <- randomRIO(1, 100)
	handleInput secret 0

