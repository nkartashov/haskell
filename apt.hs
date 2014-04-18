diff xs = do
	p <- zip xs (tail xs)
	return $ abs (fst p - snd p)

diff' xs = do
	p <- zip (tail xs) xs
	return $ abs (fst p - snd p)


