{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module AtividadeE1 (
    retornarSubString,
    split,
    aplicaRecursivo,
    aplicaFuncao
) where

-- 1
-- Uma classe é um conjunto de tipos, que por sua vez, é um conjutno de valores relacionados. Elas definem 
-- comportamentos padrões para esses tipos. Por exemplo: a classe Eq tem entre suas instâncias os tipos Int, 
-- Double, Bool ou listas que são do tipo Eq. Já de função, implementa "==" e "/=". Com isso, é possível 
-- realizar, por exemplo, operar a == b, com a :: Int e b :: Int, já que Int é instância de Eq.

-- 2
aplicaRecursivo :: (a -> a) -> [a] -> [a]
aplicaRecursivo f (x:xs)
    | null xs = [(\x -> f x) x]
    | otherwise = (\x -> f x) x : aplicaRecursivo f xs

aplicaFuncao :: (a -> b) -> [a] -> [b]
aplicaFuncao f lista = map (\x -> f x) lista

-- 3
retornarSubString :: Int -> Int -> String -> String
retornarSubString _ _ [] = []
retornarSubString inicio fim (x:xs)
    | inicio /= 0 = retornarSubString (inicio - inicio) (fim-inicio) (drop (inicio - 1) (x:xs))
    | fim == 0 = [x]
    | otherwise = x : retornarSubString inicio (fim-1) xs

-- 4
split :: String -> Char -> [String]
split [] _ = []
split (x:xs) caractere
    | x == caractere = split xs caractere
    | otherwise = parte : split (drop tamanho (x:xs)) caractere
        where
            parte = takeWhile (/= caractere) (x:xs)
            tamanho = length parte + 1