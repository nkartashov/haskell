module Nat where

-- (1 балл)

data Nat = Zero | Suc Nat

instance Eq Nat where
	Zero == Zero = True
	Zero == _ = False
	_ == Zero = False
	(Suc x) == (Suc y) = x == y

instance Ord Nat where
		compare Zero Zero = EQ
		compare Zero _ = LT
		compare _ Zero = GT
		compare (Suc x) (Suc y) = compare x y

instance Num Nat where
	Zero + Zero = Zero
	Zero + x = x
	(Suc x) + y = Suc (x + y)

	Zero * _ = Zero
	(Suc Zero) * x = x
	(Suc x) * y = y + (x * y)
	
	signum Zero = Zero
	signum _ = Suc Zero
   
	fromInteger 0 = Zero
	fromInteger x = Suc $ fromInteger (x - 1)
    
	negate Zero = Zero
	negate _ = error "Nat: negative value"
    
	abs = id

instance Show Nat where
  show = go 0 
			where 
					go acc Zero = show acc 
					go acc (Suc x) = go (acc + 1) x

