while :: Monad m => m Bool -> m () -> m ()
while cond body = do
	value <- cond
	case value of
		True -> body >> while cond body
		False -> return ()
