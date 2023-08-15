module Prova1 (
    encontrarMenor,
    apagarMenor,
    converteSegundos
) where

-- dependentes
-- remover menor
-- calcular hora
-- lista de funções

encontrarMenor [] menor = menor
encontrarMenor (x:xs) menor
    | x < menor = encontrarMenor xs x
    | otherwise = encontrarMenor xs menor

apagarMenor [] = []
apagarMenor (x:xs)
    | x == menor = xs
    | otherwise = [x] ++ apagarMenor xs
        where
            menor = encontrarMenor (x:xs) x

-- 3
converteHora s
    | s >= 3600 = show (s `div` 3600)
    | otherwise = "00"

converteMinuto s
    | s >= 60 = show (s `div` 60)
    | otherwise = "00"

converteSegundos s = horas ++ ":" ++ minutos ++ ":" ++ segundos
    where
        horas = converteHora s
        minutos = converteMinuto (s `mod` 3600)
        segundos = show (s `mod` 60)