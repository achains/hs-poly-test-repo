module Part1.Tasks where

import Util(notImplementedYet)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin = notImplementedYet

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos = notImplementedYet

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD x y = myGCD' (abs x) (abs y)
    where 
    myGCD' x 0 = x 
    myGCD' x y = myGCD' y (x `mod` y)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isLeapYear :: Integer -> Bool 
isLeapYear yy = ((yy `mod` 4 == 0) && (yy `mod` 100 /= 0)) || (yy `mod` 400 == 0)

isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect dd mm yy 
    | mm < 1 || mm > 12 = False 
    | dd < 1 || dd > 31 = False 
    | isLeapYear yy && mm == 2 = dd <= 29 
    | mm == 2 = dd <= 28
    | mm == 3 || mm == 6 || mm == 9 || mm == 11 = dd <= 31 
    | otherwise = dd <= 30

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow n k = if k == 0 then 1 else n * myPow n (k-1)

-- является ли данное число простым?
isPrime' n k
    | k == n = True
    | otherwise = if (n `mod` k == 0) then False else isPrime' n (k + 1)  

isPrime :: Integer -> Bool
isPrime n 
    | n < 2 = False
    | otherwise = isPrime' n 2

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea = notImplementedYet

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = notImplementedYet
