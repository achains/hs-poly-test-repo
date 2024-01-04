module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs 


-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs) 

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = 
    let helper x acc = f x : acc 
    in myFoldr helper []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = 
    let helper x acc = f x ++ acc
    in myFoldr helper [] 

myConcat :: [[a]] -> [a]
myConcat = 
    let helper x acc = myFoldr (:) acc x
    in myFoldr helper []

myReverse :: [a] -> [a]
myReverse = 
    let helper x acc = acc : x 
    in myFoldl helper []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = 
    let helper x acc | p x = x : acc
                     | otherwise = acc
    in myFoldr helper []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = 
    let helper x (tacc, facc) | p x       = (x : tacc, facc)
                              | otherwise = (tacc, x : facc)
    in myFoldr helper ([], [])


