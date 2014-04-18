module Expr where

import qualified Data.Map as M

-- (3 балла)

data Value = I Integer | B Bool deriving (Eq, Show)
data BinOp = Plus | Mul | Minus | Less | Greater | Equals
data UnOp = Neg | Not
data Expr = BinOp BinOp Expr Expr | UnOp UnOp Expr | Const Value | Var String
data Statement = Assign String Expr | If Expr Program (Maybe Program) | While Expr Program
type Program = [Statement]

infixr 0 $=
($=) = Assign

infix 4 `less`, `greater`
less = BinOp Less
greater = BinOp Greater

infixl 7 `mul`
mul = BinOp Mul

infixl 6 `plus`, `minus`
plus = BinOp Plus
minus = BinOp Minus

infix 1 `else_`
else_ :: (Maybe Program -> Statement) -> Program -> Statement
else_ f e = f (Just e)

type Error = String

-- evalExpr m e интерпретирует e в контексте m, который сопоставлят имени переменной ее значение.
-- evalExpr возвращает либо успешно вычисленный результат, либо список ошибок.
-- Ошибки бывают двух видов: необъявленная переменная и несоответствие типов.
evalExpr :: M.Map String Value -> Expr -> Either [Error] Value
evalExpr _ (Const x) = Right x
evalExpr context (Var name)
	| not $ M.member name context = Left ["Variable " ++ name ++ " is undefined"]
	| otherwise = Right $ context M.! name

evalExpr _ (UnOp Not (Const (I _))) = Left ["Not cannot apply to ints"]
evalExpr _ (UnOp Neg (Const (B _))) = Left ["Neg cannot apply to bools"]
evalExpr _ (BinOp _ (Const (B _)) (Const (I _))) = Left ["Cannot apply binary operation to int & bool"]
evalExpr _ (BinOp _ (Const (I _)) (Const (B _))) = Left ["Cannot apply binary operation to bool & int"]

evalExpr _ (BinOp Plus (Const (B _)) (Const (B _))) = Left ["Cannot apply plus to bools"]
evalExpr _ (BinOp Minus (Const (B _)) (Const (B _))) = Left ["Cannot apply minus to bools"]
evalExpr _ (BinOp Mul (Const (B _)) (Const (B _))) = Left ["Cannot apply mul to bools"]

evalExpr _ (UnOp Not (Const (B b))) = Right $ B $ not b
evalExpr _ (UnOp Neg (Const (I i))) = Right $ I (-i)

evalExpr _ (BinOp Plus (Const (I i1)) (Const (I i2))) = Right $ I $ i1 + i2
evalExpr _ (BinOp Minus (Const (I i1)) (Const (I i2))) = Right $ I $ i1 - i2
evalExpr _ (BinOp Mul (Const (I i1)) (Const (I i2))) = Right $ I $ i1 * i2
evalExpr _ (BinOp Less (Const (I i1)) (Const (I i2))) = Right $ B $ i1 < i2
evalExpr _ (BinOp Greater (Const (I i1)) (Const (I i2))) = Right $ B $ i1 > i2
evalExpr _ (BinOp Equals (Const (I i1)) (Const (I i2))) = Right $ B $ i1 == i2

evalExpr _ (BinOp Equals (Const (B b1)) (Const (B b2))) = Right $ B $ b1 == b2

evalExpr context (UnOp op e) = go $ evalExpr context e
	where 
		go (Left x) = Left x
		go (Right x) = evalExpr context (UnOp op $ Const x)

evalExpr context (BinOp op e1 e2) = go (evalExpr context e1) (evalExpr context e2)
	where 
		go (Left x1) (Left x2) = Left $ x1 ++ x2
		go (Left x1) _ = Left x1
		go _ (Left x2) = Left x2
		go (Right x1) (Right x2) = evalExpr context (BinOp op (Const x1) (Const x2))

-- evalStatement m e интерпретирует e в контексте m.
-- evalStatement возвращает либо обновленный контекст, либо список ошибок.
evalStatement :: M.Map String Value -> Statement -> Either [Error] (M.Map String Value)
evalStatement context (Assign name expr) = case evalExpr context expr of
																						 Left e -> Left e
																						 Right value -> Right $ M.insert name value context

evalStatement context (If condition body Nothing) = case evalExpr context condition of
												 Left e -> Left e
												 Right (B True) -> evalProgram context body
												 Right _ -> Right context

evalStatement context (If condition body (Just otherBody)) = case evalExpr context condition of
												 Left e -> Left e
												 Right (I _) -> Left ["If cannot be applied to an integer"]
												 Right (B True) -> evalProgram context body
												 Right _ -> evalProgram context otherBody

evalStatement context (While condition body) = case evalExpr context condition of
												 Left e -> Left e
												 Right (B True) -> case evalProgram context body of
																						 Left e -> Left e
																						 Right c -> evalStatement c (While condition body)
												 Right _ -> Right context 



evalProgram :: M.Map String Value -> Program -> Either [Error] (M.Map String Value)
evalProgram context [] = Right context
evalProgram context (s:xs) = case evalStatement context s of
														 Left e -> Left e
														 Right c -> evalProgram c xs
