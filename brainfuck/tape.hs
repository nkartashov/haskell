module Tape (Tape(Tape), emptyTape, moveRight, moveLeft) where

data Tape a = Tape [a] a [a]

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
      where zeros = repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls p (r:rs)) = Tape (p:ls) r rs

moveRight (Tape _ _ []) = undefined

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) p rs) = Tape ls l (p:rs)

moveLeft (Tape [] _ _) = undefined

instance Functor Tape where
  fmap f (Tape ls p rs) = Tape (fmap f ls) (f p) (fmap f rs)
