-- Александра Михайлова, Computer Science Center, направление Data Mining.
-- Задание 2, задача 2.

-- Написать функцию maxthree :: [Integer] -> [Integer],
-- которая получает список целых и выдает список той же длины,
-- содержащий в качестве i-го элемента максимальные значения трех элементов списка - i-го, (i-1)-го и (i+1)-го.

-- Основная функция.
-- На место первого элемента в новом списке ставим max первого и второго элементов,
-- остальные элементы нового списка (кроме последнего элемента) - это максимумы соответствующих "окон"-подсписков размера 3.
-- (Последний элемент нового списка - максимум "окна" размера 2.)
maxthree :: [Integer] -> [Integer]
-- Вспомогательная функция.
-- Отвечает за "окна".
maxthree' :: [Integer] -> [Integer]
maxthree' [] = []
maxthree' [a] = []
maxthree' (x:y:ls) = (listmax (sublist (x:y:ls) 0 2)) : (maxthree' (y:ls))
maxthree [] = []
maxthree [a] = [a]
maxthree (x:y:ls) = (max x y) : (maxthree' (x:y:ls))

-- Другие вспомогательные функции.
-- Максимум в непустом списке целых чисел.
listmax :: [Integer] -> Integer
listmax [x] = x
listmax (x:ls) = max x (listmax ls)

-- Функция, возвращающая подсписок данного целочисленного списка с i-го (включительно) по j-й (включительно) элемент.
-- Аргументы: список, i - индекс начала подсписка, j - индекс конца подсписка.
sublist :: [Integer] -> Int -> Int -> [Integer]
sublist [] i j = []
sublist (x:ls) i j | i > j = []
	| i < 0 = sublist (x:ls) 0 j
	| j >= length (x:ls) = sublist (x:ls) i (length(x:ls) - 1)
	| (i == 0 && i == j) = [x]
	| i == 0 = x : (sublist ls i (j - 1))
	| otherwise = sublist ls (i - 1) (j - 1)

-- Тесты.
test = (maxthree [3, 8, 6, 5, 1], maxthree [1, 2], maxthree[2], maxthree [])
