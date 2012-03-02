-- Александра Михайлова, Computer Science Center, направление Data Mining.
-- Задание 2, задача 3.

-- Написать функцию removeEven :: [String] -> [String], которая по заданному списку строк строит список,
-- в котором содержатся те же строки, что и в исходном списке, но каждая вторая строка из списка выброшена,
-- а в оставшихся строках выброшен каждый второй символ.

-- Основная функция.
-- Аргумент - данный нам список.
-- Используются стандартные функции filter и map.
removeEven :: [String] -> [String]
removeEven ls = map f (f ls) 

-- Вспомогательные функции.
-- Функция, используемая для фильтрации исходного списка от "четных" слов на первом этапе (filter)
-- и для изменения элементов отфильтрованного списка (map) на втором этапе.
f :: [a] -> [a]
f [] = []
f ls = denumerate (filter fits (enumerate ls))
-- Функция для фильтрации.
fits :: (Int, a) -> Bool
fits elem = ((fst elem) `mod` 2 == 1)
-- Функция, создающая пронумерованный список на основе заданного списка.
enumerate :: [a] -> [(Int, a)]
enumerate' :: [a] -> Int -> [(Int, a)]
enumerate' [] i = []
enumerate' (x:ls) i = (i, x) : (enumerate' ls (i + 1))
enumerate (x:ls) = enumerate' (x:ls) 1
-- Обратная функция (денумерация списка).
denumerate :: [(Int, a)] -> [a]
denumerate [] = []
denumerate (x:ls) = (snd x) : denumerate ls

-- Тесты.
test = (removeEven ["one", "two", "three", "four"], removeEven [], removeEven["one"])

 

