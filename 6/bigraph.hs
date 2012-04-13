-- Александра Михайлова, Computer Science Center, направление Data Mining.
-- Задание 6, задача 2.

type Graph = (Int, Int -> Int -> Bool)

-- Написать функцию bigraph :: Graph -> Bool,
-- которая проверяет, является ли заданный связный неориентированный граф двудольным.

-- Функциональное представление множества.
type Set = (Int -> Bool)

-- Функциональное представление операций над множествами и некоторых объектов.
-- Пустое множество.
emptySet :: Set
emptySet x = False

-- Объединение множеств.
(+++) :: Set -> Set -> Set
(s1 +++ s2) x = s1 x || s2 x

-- Пересечение множеств.
(&&&) :: Set -> Set -> Set
(s1 &&& s2) x = s1 x && s2 x

-- Разность множеств.
(\\) :: Set -> Set -> Set
(s1 \\ s2) x = s1 x && not (s2 x)

-- Проверка множества на пустоту.
-- Аргументы: s - множество, n - число вершин графа (что эквивалентно множеству вершин графа,
-- так как они нумеруются от 1 до n).
isEmpty :: Set -> Int -> Bool
isEmpty s n = not (any s [1..n])

-- Основная функция.
-- Проверка графа на двудольность.
bigraph :: Graph -> Bool
bigraph = go (== 1) emptySet (== 1) (== 1)

-- Функция go выполняет обход графа и проверяет его на двудольность.
-- (Проверяет, можно ли разделить вершины на два множества, fst и snd.)
-- Возвращает True, если граф двудолен, и False в противном случае.
go :: Set -> Set -> Set -> Set -> Graph -> Bool
go fst snd passed front graph@(n,f)
      | not (isEmpty (fst &&& snd) n) = False
	 | isEmpty front n = True
	 | otherwise =  go (snd +++ newFront) fst newPassed
                     (newFront \\ newPassed) graph
     where newFront = foldr (+++) emptySet (map f (filter front [1..n]))
           newPassed = passed +++ front
