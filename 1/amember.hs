-- Александра Михайлова, Computer Science Center, направление Data Mining.
-- Задание 1, задача 1.

-- Написать функцию amember :: Double -> Double -> Int -> Double,
-- возвращающую n-ый член арифметической прогрессии
-- (члены прогрессии нумеруются с нуля).

-- Основная функция.
-- Аргументы: first - первый член прогрессии, step - шаг прогрессии,
-- n - номер искомого члена прогрессии.
-- Результат: n-й член данной прогрессии.
amember :: Double -> Double -> Int -> Double
amember first step n | n < 0 = error "Negative member's number"
	| n == 0 = first
	| otherwise = amember (first + step) step (n - 1)

-- Тесты.

-- Первый, второй и третий тесты проверяют правильность вычислений.
-- Результаты должны быть равны 11, 1.8 и -20 соответственно.
-- Четвертый тест подтверждает работоспособность программы при задании большого аргумента.
-- Результат должен быть равен 1501.
test = (amember 1 2 5, amember 0.0 0.3 6, amember 0 (-4) 5, amember 1 3 500)

-- Следующий тест проверяет корректность программы.
-- Результатом должна быть ошибка "Negative member's number".
correctness_test = amember 1 3 (-5)
