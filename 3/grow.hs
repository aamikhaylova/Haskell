-- Александра Михайлова, Computer Science Center, направление Data Mining.
-- Задание 3, задача 2.

-- Написать функцию grow :: [Integer] -> [[Integer]],
-- которая получает список целых и выдает список неубывающих фрагментов исходного списка.
-- Например, grow [3, 3, 8, 6, 5, 7] => [[3, 3, 8], [6], [5, 7]].

import Data.List

-- Основная функция.
-- Аргумент: список целых чисел.
-- Результат: список неубывающих фрагментов исходного списка.
-- Идея: берём первую неубывающую подпоследовательность исходной последовательности (списка), заносим её в результат,
-- затем применяем функцию рекурсивно к оставшейся части списка.
grow :: [Integer] -> [[Integer]]
grow [] = []
grow ls = (take (longestGrowingHeadLength ls) ls) : (grow (drop (longestGrowingHeadLength ls) ls))


-- Вспомогательные функции.
-- Функция, возвращающая длину первой неубывающей подпоследовательности.
longestGrowingHeadLength :: [Integer] -> Int
longestGrowingHeadLength [] = 0
longestGrowingHeadLength ls = length (head (filter isGrowing (heads ls)))

-- Функция, проверяющая последовательность целых чисел на неубывание.
isGrowing :: [Integer] -> Bool
isGrowing [] = False
isGrowing [a] = True
isGrowing (x:ls) = (x <= (head ls)) && (isGrowing ls)

-- Функция, возвращающая список всех префиксов исходного списка (от самого длинного к самому короткому).
heads :: [a] -> [[a]]
heads [] = []
heads ls = map reverse ((tails . reverse) ls)

-- Тесты.
test = (grow [3, 3, 8, 6, 5, 7], grow [], grow [1, 1, 1], grow [5, 4, 3, 2, 1])
