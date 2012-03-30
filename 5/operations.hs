-- Александра Михайлова, Computer Science Center, направление Data Mining.
-- Задание 5, задача 1.

-- Определить, можно ли расставить знаки "+", "-", "*" и круглые скобки между числами 1, 2,..., 10
-- (именно в этом порядке, без перестановок) так, чтобы в результате выполнения всех действий получилось заданное число.
-- Функция должна получать на вход число и выдавать строку, изображающую правильную расстановку знаков и скобок,
-- или строку "impossible", если заданное число получить невозможно.

-- Идея: представляем все возможные выражения в виде двоичного дерева.
data Tree = Num Int | Op Tree Char Tree
-- (Красивый) вывод дерева такого типа.
instance Show Tree where
  showsPrec _ (Num n) = (show n ++)
  showsPrec _ (Op t1 op t2) = ('(':) . (shows t1) . (op:) . (shows t2) . (')':)

-- Функция, вычисляющая выражение в дереве.
calculate :: Tree -> Integer
calculate (Num a) = fromIntegral a -- приведение типов!
calculate (Op t1 operation t2) | operation == '+' = calculate t1 + calculate t2
	| operation == '-' = calculate t1 - calculate t2
	| operation == '*' = calculate t1 * calculate t2

-- Строим все разбиения списка целых чисел на два его подсписка (т.е. "разбивающий" элемент пробегает по нашему списку).
allPairs :: [Int] -> [([Int], [Int])]
allPairs ls = map ((flip splitAt) ls) [1..(length ls - 1)]

-- Вставляем в пару подсписков заданный во втором аргументе знак операции.
insertOp :: [Int] -> Char -> [([Int], Char, [Int])]
insertOp ls operation = map (\(a,b) -> (a, operation, b)) (allPairs ls)

-- Строим все тройки из подсписков и операций '+', '-', '*'.
allTriples :: [Int] -> [([Int], Char, [Int])]
allTriples ls = concat (map (insertOp ls) ['+', '-', '*'])

-- Строим все деревья указанного выше типа, в листьях которых стоят числа из заданного в первом аргументе списка.
allTrees :: [Int] -> [Tree]
allTrees [a]  = [Num a]
allTrees ls = concat (map descartesTree (allTriples ls))
                 where build op (t1, t2) = Op t1 op t2
                       descartesTree (ls1, op, ls2) = map (build op) [(t1, t2) | t1 <- allTrees ls1, t2 <- allTrees ls2] -- TODO: красиво ? чтобы не падало на больших числах ??

-- Вспомогательная функция.
-- Нужна для того, чтобы взять первое решение (т.е. подходящую расстановку знаков) из списка всех вариантов.
-- Берём из списка ls первый элемент, удовлетворяющий заданному (в первом аргументе) условию f.
find :: (a -> Bool) -> [a] -> Maybe a
find f ls = lookup True (zip (map f ls) ls) -- TODO: как сделать красиво ?

-- Стартовая функция solve.
solve' (Just res) = show res
solve' Nothing = "impossible"
solve n = solve' (find (\t -> (calculate t == n)) (allTrees [1..10]))

-- Тест.
test = (solve 20, solve 201)
