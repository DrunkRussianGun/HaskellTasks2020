module Part3 where

import Data.List.NonEmpty (group, toList)

------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 number = primeDivisors number == [number]

primeDivisors :: Integer -> [Integer]
primeDivisors = primeDivisorsFrom 2
	where
		primeDivisorsFrom :: Integer -> Integer -> [Integer]
		primeDivisorsFrom _ 1 = []
		primeDivisorsFrom minDivisor number
			| minDivisor * minDivisor > number = [number]
			| number `rem` minDivisor == 0
				= minDivisor : primeDivisorsFrom minDivisor (number `div` minDivisor)
			| otherwise = primeDivisorsFrom (minDivisor + 1) number

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 number = let
		groupings = (map toList . group . primeDivisors) number
	in map (\ grouping -> (head grouping, length grouping)) groupings

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 number = sum ((init . divisors) number) == number

divisors :: Integer -> [Integer]
divisors number = divisorsFrom 1 number
	where
		divisorsFrom :: Integer -> Integer -> [Integer]
		divisorsFrom minDivisor number
			| minDivisor * minDivisor > number = []
			| minDivisor * minDivisor == number = [minDivisor]
			| number `rem` minDivisor == 0
				= [minDivisor] ++ divisorsFrom (succ minDivisor) number ++ [number `div` minDivisor]
			| otherwise = divisorsFrom (succ minDivisor) number

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 = divisors

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 = toInteger . product . map lettersCount . words
	where
		lettersCount :: String -> Int
		lettersCount = length . filter (== 'i')

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 input = let
		(startIndex, afterStartIndex) = head (reads input :: [(Int, String)])
		(endIndex, afterEndIndex) = head (reads (drop 1 afterStartIndex) :: [(Int, String)])
		string = drop 2 afterEndIndex
		leftIndex = min startIndex endIndex
		rightIndex = max startIndex endIndex
	in if leftIndex <= length string && rightIndex <= length string
		then let
				order = if startIndex <= endIndex then (\ x -> x) else reverse
			in Just $ (order . take (rightIndex - leftIndex + 1) . drop (leftIndex - 1)) string
		else Nothing

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 k = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 = error "Implement me!"
