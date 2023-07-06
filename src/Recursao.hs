{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Recursao (
    fatorial,
    somar,
    exponenciacao,
    euclides,
    verificarTrue,
    concatenarLista,
    replicar,
    encontrarElemento,
    verificarElemento,
    merge,
    metades,
    mergesort,
    somarListaInteiros,
    calcularElementosLista,
    ultimoElementoLista
) where

-- Q1
fatorial :: Integer -> Integer
fatorial x
    | x < 1 = error "Informe um inteiro maior ou igual a 1."
    | x == 1 = 1
    | otherwise = x * fatorial (x - 1)

-- Q2
somar :: (Eq t, Num t) => t -> t
somar x
    | x == 0 = 0
    | otherwise = x + somar (x - 1)

-- Q3
exponenciacao :: (Eq a, Num a) => a -> a-> a
exponenciacao m n
    | n == 0 = 1
    | otherwise = m * exponenciacao m (n-1)

-- Q4
euclides :: Integral t => t -> t -> t
euclides a b = if resto == 0 then b else euclides b resto
    where
        resto = mod a b

-- Q5 A
verificarTrue :: [Bool] -> Bool
verificarTrue (x:xs)
    | null xs = x
    | otherwise =  x && verificarTrue xs

-- Q5 B
concatenarLista :: [[a]] -> [a]
concatenarLista (x:xs)
    | null xs = head [x]
    | otherwise = head (x:xs) ++ concatenarLista xs

-- Q5 C
replicar :: Int -> a -> [a]
replicar 0 _ = []
replicar n e = if n == 1 then [e] else e : replicar (n-1) e

-- Q5 D
encontrarElemento :: [a] -> Int -> a
encontrarElemento (x:xs) n
    | n == 1 = x
    | otherwise = encontrarElemento xs (n-1)

-- Q5 E
verificarElemento :: Eq a => a -> [a] -> Bool
verificarElemento e (x:xs)
    | e == x = True
    | null xs = False
    | otherwise = verificarElemento e xs

-- Q6
merge :: Ord a => [a] -> [a] -> [a]
merge list [] = list
merge [] list = list
merge (x:xs) (y:ys)
    | x == y = x : y : merge xs ys
    | x > y = y : merge (x:xs) ys
    | x < y = x : merge xs (y:ys)

-- Q7
metades :: [a] -> ([a], [a])
metades list = splitAt (div (length list) 2) list

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort list = merge (mergesort (fst metade)) (mergesort (snd metade))
        where
            metade = metades list

-- Q8 A
somarListaInteiros :: [Int] -> Int
somarListaInteiros (x:xs)
    | null xs = x
    | otherwise = x + somarListaInteiros xs

-- Q8 B

calcularElementosLista :: [a] -> Int
calcularElementosLista (_:xs)
    | null xs = 1
    | otherwise = 1 + calcularElementosLista xs

-- Q8 C

ultimoElementoLista :: [a] -> a
ultimoElementoLista (x:xs)
    | null xs = x
    | otherwise = ultimoElementoLista xs
