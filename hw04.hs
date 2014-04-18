-- 1. fib n вовзращает n-ое число Фибоначчи
--    (1 балл)
fib :: Integer -> Integer
fib n = helper n 0 1
	where helper n f1 f2
		| n == 0    = f1
		| otherwise = helper (n - 1) f2 (f1 + f2)

-- 2a. Написать функцию, возвращающую количество цифр числа.
--     Для целочисленного деления можете использовать функции div и mod.
--    (0.5 балла)
numberOfDigits :: Integer -> Integer
numberOfDigits n = helper 0 n
	where helper res n 
					| n < 10    = res + 1 
					| otherwise = helper (res + 1) $ div n 10

-- 2b. Написать функцию, возвращающую сумму цифр числа.
--    (0.5 балла)
sumOfDigits :: Integer -> Integer
sumOfDigits n = helper 0 n
	where helper res n 
					| n < 10    = res + n 
					| otherwise = helper (res + (n - (div n 10) * 10)) $ div n 10


-- 3. gcd' возвращает НОД.
--    (1 балл)
gcd' :: Integer -> Integer -> Integer
gcd' b a
	| b < a     = gcd a b
	| a == 0    = b
	| otherwise = gcd a $ mod b a


-- 4. minp p возвращает минимальное по модулю число x такое, что p x == True. Если такого x не существует, minp не завершается.
--    (1 балл)
minp :: (Integer -> Bool) -> Integer
minp p = helper p 0
	where helper p bound
					| (p bound == True)    = bound
					| (p (-bound) == True) = -bound
					| otherwise            = helper p $ bound + 1

-- 5. integral f a b возвращает значение определенного интеграла функции f на отрезке [a,b].
--    Для реализации можете использовать метод трапеций.
--    (2 балла)
integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b = helper f a b 0
	where helper f a b res
					| (abs (b - a)) < step = res + (((f b) + (f a)) / 2 * (b - a))
					| otherwise            = helper f (a + step) b (res + (((f a) + (f (a + step))) / 2 * step))
					where step = 0.001

-- 6. Реализуйте оператор примитивной рекурсии rec, используя функцию (-), укажите тип rec.
--    (1 балл)
rec :: a -> (Integer -> a -> a) -> Integer -> a
rec initState body counter
	| counter == 0 = initState 
	| otherwise    = body (counter - 1) (rec initState body (counter - 1))

-- 7. Реализуйте факторил при помощи rec.
--    (1 балл)
facRec :: Integer -> Integer
facRec n = rec 1 (\x y -> if x == 0 then y else x * y) (n + 1)

-- 8. Реализуйте факториал при помощи fix.
--    (1 балл)
facFix :: Integer -> Integer
facFix = fix (\fun -> \n -> if n == 1 then 1 else n * (fun (n - 1)))
	where fix f = f (fix f)
