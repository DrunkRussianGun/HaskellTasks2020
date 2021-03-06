module Part1
  ( prob1
  , prob2
  , prob3
  , prob4
  , prob5
  ) where

------------------------------------------------------------
-- PROBLEM #1
--
-- Реализовать функцию, которая возвращает остаток от
-- деления на 65537 суммы утроенного своего аргумента
-- и числа 123
--
-- На вход функции подаются неотрицательные числа
prob1 :: Int -> Int
prob1 x = (x * 3 + 123) `mod` 65537


------------------------------------------------------------
-- PROBLEM #2
--
-- Реализовать функцию, которая:
-- * нечётные числа увеличивает втрое и добавляет единицу
-- * чётные числа делит на два
prob2 :: Integer -> Integer
prob2 n = case n `mod` 2 == 0 of
	False -> n * 3 + 1
	True  -> n `div` 2


------------------------------------------------------------
-- PROBLEM #3
--
-- Реализовать функцию, которая принимает функцию step,
-- положительное число n и пока текущее число не станет
-- равно единице:
-- * вызывает step с текущим числом для получения
--   следующего числа
-- * если текущее число -- единица, возвращает количество
--   выполненных шагов
--
-- Например, если в качестве step используется уменьшение
-- на единицу, а в качестве n передать 5, то должно быть
-- возвращено 4, поскольку последовательность будет такой:
--    5 -> 4 -> 3 -> 2 -> 1
--
-- Если в качестве step передать решение prob2, а n == 3,
-- то ответ 7, а последовательность такая:
--    3 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
--
-- Для любой функции step и n == 1 ответом будет 0.
prob3 :: (Integer -> Integer) -> Integer -> Integer
prob3 _ 1 = 0
prob3 step n = prob3 step (step n) + 1


------------------------------------------------------------
-- PROBLEM #4
--
-- Реализовать функцию, возвращающую n-е число Фибоначчи.
-- Нулевое число равно 1, первое тоже 1. Каждое последующее
-- равно сумме двух предыдущих.
--
-- Число n может быть отрицательным, последовательность
-- продолжается естественным образом: (-1)-е число равно 0,
-- далее (-2)-е равно 1, (-3)-е равно (-1), (-4)-е равно 2
-- и т.д. -- сохраняется свойство, что последующие числа
-- равны сумме двух предыдущих.
--
-- Число n по модулю не превосходит 10^5
prob4 :: Integer -> Integer
prob4 index
	| index < 0 = negativeFibonacci 1 1 index
	| otherwise = positiveFibonacci 1 1 index
		where
			-- first > second > third => first = second + third => third = first - second
			negativeFibonacci first second index = case index of
				0 -> second
				otherwise -> negativeFibonacci second (first - second) (succ index)
			-- first < second < third => third = first + second
			positiveFibonacci first second index = case index of
				0 -> first
				otherwise -> positiveFibonacci second (first + second) (pred index)

------------------------------------------------------------
-- PROBLEM #5
--
-- Написать функцию, возвращающую True, если все простые
-- делители первого аргумента n меньше второго аргумента k
--
-- Числа n и k положительны и не превосходят 10^8.
-- Число 1 не считается простым числом
prob5 :: Integer -> Integer -> Bool
prob5 n bound = all (< bound) (primeDivisors n)
	where
		primeDivisors :: Integer -> [Integer]
		primeDivisors = divisorsFrom 2

		divisorsFrom :: Integer -> Integer -> [Integer]
		divisorsFrom _ 1 = []
		divisorsFrom minDivisor number
			| minDivisor * minDivisor > number = [number]
			| number `rem` minDivisor == 0 = minDivisor : divisorsFrom minDivisor (number `div` minDivisor)
			| otherwise = divisorsFrom (minDivisor + 1) number
