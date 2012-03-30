-- Александра Михайлова, Computer Science Center, направление Data Mining.
-- Задание 5, задача 2.

-- Найти все способы разбиения заданной строки символов на слова из заданного словаря
-- или определить, что такого разбиения не существует.
-- Функция должна получать в качестве аргументов исходную строку и список попарно несовпадающих слов (словарь),
-- а в результате выдавать список возможных разбиений
-- (каждое разбиение — это список слов из словаря, при соединении которых получается исходная строка).

-- Основная функция.
-- Аргументы: str - строка, dictionary - словарь.
-- Идея: смотрим, с каких слов словаря может начинаться строка,
-- потом каждое из этих слов присоединяем к результату рекурсивного вызова функции для остатка строки.
dict :: String -> [String] -> [[String]]
dict "" dictionary = [[]]
dict str [] = [[str]]
dict str dictionary | getPrefixes str dictionary == [] = []
	| otherwise = concat (map app (getPrefixes str dictionary))
		where app word = map (word:) (dict (drop (length word) str) dictionary)

-- Вспомогательные функции.

-- Функция, определяющая, является ли слово word префиксом строки str.
isPrefix :: String -> String -> Bool
isPrefix str word = ((length word) <= (length str)) && ((take (length word) str) == word)

-- Функция выдает список слов из словаря dictionary, с которых начинается строка str (т.е. список всех префиксов str, входящих в dictionary).
getPrefixes :: String -> [String] -> [String]
getPrefixes str dictionary = filter (isPrefix str) dictionary

-- Тест.
test = (dict "to be or not to be" [" ", "a", "be", "is", "or", "one", "not", "that", "to"], dict "" ["some", "dictionary"], dict "some string" [])
