module Part2 where

import Part2.Types

import Data.Maybe

------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 letter = case letter of
	RED   -> 'R'
	GREEN -> 'G'
	BLUE  -> 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 part = case part of
	Red   partValue -> checkValue partValue
	Green partValue -> checkValue partValue
	Blue  partValue -> checkValue partValue
	where checkValue value = 0 <= value && value <= 255

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 color part = case part of
	Red   value -> Color (red color + value) (green color)         (blue color)
	Green value -> Color (red color)         (green color + value) (blue color)
	Blue  value -> Color (red color)         (green color)         (blue color + value)

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 part = case part of
	Red   value -> value
	Green value -> value
	Blue  value -> value

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 color = let
	(maxPart, countOfMaxima) = foldl
		(\ (maxPart, countOfMaxima) nextPart -> case (prob9 maxPart) `compare` (prob9 nextPart) of
			LT -> (nextPart, 1)
			EQ -> (maxPart, countOfMaxima + 1)
			GT -> (maxPart, countOfMaxima))
		(Red $ red color, 1)
		[Green $ green color, Blue $ blue color]
	in if countOfMaxima == 1
		then Just $ maxPart
		else Nothing

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 tree = sumOfBranch (left tree) + root tree + sumOfBranch (right tree)
	where sumOfBranch branch = maybe 0 prob11 branch

------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
prob12 tree = checkTree (Just tree) Nothing Nothing
	where checkTree :: Ord a => Maybe (Tree a) -> Maybe a -> Maybe a -> Bool
	      checkTree tree minValue maxValue = isNothing tree ||
	      	(let rootValue = (root . fromJust) tree
	      	in (isNothing minValue || fromJust minValue <= rootValue) &&
	      	(isNothing maxValue || rootValue < fromJust maxValue) &&
	      	checkTree (left $ fromJust tree)  minValue (Just rootValue) &&
	      	checkTree (right $ fromJust tree) (Just rootValue) maxValue)

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 value tree = findValue value (Just tree)
	where findValue value tree = maybe
		Nothing
		(\ tree -> case value `compare` root tree of
			LT -> findValue value (left tree)
			EQ -> Just tree
			GT -> findValue value (right tree))
		tree

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 tree = fromJust $ replaceBrackets (Just tree) Nothing
	where replaceBrackets tree value = maybe
		Nothing
		(\ tree -> let
				newRightTree = replaceBrackets (right tree) value
				rightValue = maybe value (\ branch -> Just $ root branch + 1) newRightTree
				newLeftTree = replaceBrackets (left tree) rightValue
				leftValue = maybe rightValue (\ branch -> Just $ root branch + 1) newLeftTree
				rootValue = fromMaybe (1 :: Int) leftValue
			in Just $ Tree newLeftTree rootValue newRightTree)
		tree

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 oldTree = maybe
	oldTree
	(\ rightSubtree -> rightSubtree { left = Just $ oldTree { right = left rightSubtree } })
	(right oldTree)

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 oldTree = maybe
	oldTree
	(\ leftSubtree -> leftSubtree { right = Just $ oldTree { left = right leftSubtree } })
	(left oldTree)

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 = error "Implement me!"
