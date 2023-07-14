{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ListaAltaOrdem (
    impares,
    posicao,
    replicar,
    repete,
    inverter,
    palindromo,
    fibonacci,
    listaFibonacci,
    all2,
    any2,
    takeWhile2,
    dropWhile2,
    dec2int,
    altMap
) where
import Recursao (mergesort)

-- Q1
impares :: [Int] -> [Int]
impares lista = mergesort [x | x <- lista , odd x]

-- Q2
posicao :: Eq a => Int -> [a] -> a
posicao n (x:xs)
    | n == 0 = x
    | otherwise = posicao (n-1) xs

-- Q3
replicar :: Int -> Int -> [Int]
replicar 0 _ = []
replicar n x = x : replicar (n-1) x

repete :: Int -> [[Int]]
repete 1 = [[1]]
repete n = replicar n n : repete (n - 1)

-- Q4
inverter :: [a] -> [a]
inverter lista
    | null lista = []
    | otherwise = last lista : inverter (init lista)

palindromo :: (Eq a) => [a] -> Bool
palindromo lista = lista == inverter lista

-- Q5
fibonacci :: Int -> Int
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci n = fibonacci (n-2) + fibonacci (n-1)

listaFibonacci :: Int -> [Int]
listaFibonacci 0 = []
listaFibonacci n = listaFibonacci (n-1) ++ [fibonacci n]

-- Q6 A
all2 :: (a -> Bool) -> [a] -> Bool
all2 f lista = and [f x | x <- lista]

-- Q6 B
any2 :: (a -> Bool) -> [a] -> Bool
any2 f lista = or [f x | x <- lista]

-- Q6 C
takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 f (x:xs)
    | f x = x : takeWhile2 f xs
    | otherwise = []

-- Q6 D
dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 f (x:xs)
    | f x = dropWhile2 f xs
    | otherwise = x:xs

-- Q8
dec2int :: [Int] -> Int
dec2int lista = read (foldl (++) "" (map show lista)) :: Int 

-- Q10
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs)
    | even (length xs) = f x : altMap g f xs
    | otherwise = f x : altMap g f xs
