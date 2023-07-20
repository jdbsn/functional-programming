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
    mapFold,
    filterFold,
    dec2int,
    mapUnfold,
    iterateUnfold,
    altMap,
    curry1,
    uncurry1
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

-- Q7
mapFold :: (a -> b) -> [a] -> [b]
mapFold _ [] = []
mapFold f (x:xs) = foldr (\y ys -> (f y) : ys) [] xs

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold f (x:xs) = foldr (\x xs -> if f x then x:xs else xs) [] xs

-- Q8
dec2int :: [Int] -> Int
dec2int lista = read (foldl (++) "" (map show lista)) :: Int

-- Q9
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
       | p x = []
       | otherwise = h x : unfold p h t (t x)

mapUnfold :: Eq b => (b -> a) -> [b] -> [a]
mapUnfold f = unfold (== []) (f . head) tail

iterateUnfold :: (Ord a, Num a) => (a -> a) -> a -> [a]
iterateUnfold = unfold (<= 0) id

-- Q10
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs)
    | even (length xs) = f x : altMap g f xs
    | otherwise = f x : altMap g f xs

-- Q11
curry1 :: ((a, b) -> c) -> a -> b -> c
curry1 f x y = f (x,y)

uncurry1 :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
uncurry1 f (x,y) = f x y
