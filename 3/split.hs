-- Александра Михайлова, Computer Science Center, направление Data Mining.
-- Задание 3, задача 1.

-- Написать функцию split :: String -> Char -> [String], которая разрезает исходную строку на куски,
-- разделенные в исходной строке символом, указанным в качестве второго аргумента функции.
-- Например, split "one,two,,three," ',' => ["one", "two", "", "three", ""].

import Data.List

-- Основная функция.
-- Аргументы: строка (str), разделительный символ (separator).
-- Результат: куски str, разделенные в str символом separator.
-- Рассматриваем все подстроки исходной строки, начинающиеся с разделительного символа (за это отвечает filter).
-- Из каждой такой строки забираем ее подстроку, заключенную между первым и вторым разделительными символами
-- (за это отвечает map).
split :: String -> Char -> [String]
split "" _ = []
split str separator = map (takeWhile (/= separator) . tail) (filter (isPrefixOf [separator]) (tails (separator : str)))

-- Тесты.
test = (split "one,two,,three," ',', split "" ',', split "alexandra" 'q')
