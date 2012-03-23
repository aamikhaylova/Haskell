-- Александра Михайлова, Computer Science Center, направление Data Mining.
-- Задание 4, задача 3.

-- Написать функцию для перемножения двух числовых матриц.

import Data.List

-- Матрица.
type Matrix a = [[a]]

-- Столбец / строка матрицы.
type Vect a = [a]

-- Основная функция.
-- Аргументы - две матрицы (предполагается, что количество стоблцов первой = количеству строк второй).
-- Результат - произведение данных матриц.
-- Идея: берем каждую строчку первой матрицы, векторно умножаем ее на каждый столбец сторой матрицы
-- (т.е. каждую строчку транспонированной m2), в итоге получаем строчку произведения матриц.
multiplyMatrices :: Num a => Matrix a -> Matrix a -> Matrix a
multiplyMatrices m1 m2 = map (\row -> map (\column -> multiplyVectors row column) (transpone m2)) m1

-- Вспомогательные функции.

-- Функция для транспонирования матрицы.
transpone :: Matrix a -> Matrix a
transpone = foldr (zipWith (:)) (repeat [])

-- Перемножение векторов (предполагается, что длины векторов совпадают).
multiplyVectors :: Num a => Vect a -> Vect a -> a
multiplyVectors v1 v2 = sum (zipWith (*) v1 v2)

-- Тест.
test = multiplyMatrices [[1, 0, 0], [0, 1, 0], [0, 0, 1]] [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
