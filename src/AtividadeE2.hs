{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AtividadeE2 (
    funcionarios,
    prefixos,
    checaPrimo,
    primos,
    primosAlt,
    tailIf,
    tailGuard,
    tailPadrao,
    zip',
    zipWith'
) where

-- João Neto

-- Q1
funcionarios :: Foldable t => [(a1, t a2)] -> [a1]
funcionarios l = [x | (x, y) <- l, length y >= 2]

-- Q2
aux :: [a] -> Int -> [[a]]
aux lista v
    | v == length lista + 1 = []
    | otherwise = [take v lista] ++ aux lista (v+1)

prefixos :: [a] -> [[a]]
prefixos lista = aux lista 1

-- Q3
resto :: Integral a => a -> a -> Bool
resto x y = x `mod` y == 0

checaPrimo :: Int -> Bool
checaPrimo x = length (filter (resto x) [2..x]) == 1

primos :: Int -> Int -> [Int]
primos inicio fim
    | inicio > fim = filter checaPrimo [fim..inicio]
    | otherwise = filter checaPrimo [inicio..fim]

primosAlt inicio fim = [x | x <- [inicio..fim], checaPrimo x]

-- Q4
-- a)
tailIf :: [a] -> [a]
tailIf l = if null l then [] else drop 1 l

-- b)
tailGuard :: [a] -> [a]
tailGuard l
    | null l = []
    | otherwise = drop 1 l

-- c)
tailPadrao :: [a] -> [a]
tailPadrao [] = []
tailPadrao l = drop 1 l

-- Q5
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys