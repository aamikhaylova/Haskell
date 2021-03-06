-- Александра Михайлова, Computer Science Center, направление Data Mining.
-- Задание 1, задача 2.

-- Написать функцию sumdiv :: Integer -> Integer,
-- выдающую сумму всех делителей заданного положительного числа,
-- включая единицу, но исключая само это число.

-- Основная функция.
-- Аргумент: n - заданное число.
-- Результат: сумма всех делителей заданного числа n, включая единицу, но исключая n.
sumdiv :: Integer -> Integer

-- Вспомогательная функция.

-- Аргументы: n - заданное число, d - (очередной) кандидат в его делители.

-- Результат: сумма всех делителей заданного числа n, включая единицу, но исключая n.

-- Идея: проверяем все числа от 1 до целой части числа n/2 на предмет того,
-- являются ли они делителями числа n.
-- Если число является делителем n, прибавляем его к текущей сумме делителей.
-- Если нет, переходим к проверке следующего за ним числа.
factors :: Integer -> Integer -> Integer

factors n d | d * 2 > n = 0
	| n `mod` d == 0 = d + factors n (d + 1)
	| otherwise = factors n (d + 1)
 
sumdiv n | n <= 0 = error "Non-positive argument"
	 | otherwise = factors n 1

-- Тесты.

-- Первый, второй и третий тесты проверяют правильность вычислений.
-- Результаты должны быть равны 0, 1 и 55 соответственно.
-- Третий тест подтверждает работоспособность программы при задании большого аргумента.
test = (sumdiv 1, sumdiv 2, sumdiv 36, sumdiv 1000)

-- Следующий тест проверяет корректность программы.
-- Результатом должна быть ошибка "Non-positive argument".
correctness_test = sumdiv (-5)
