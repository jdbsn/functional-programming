module Lista1 (
   qtdIguais,
   calcularMedia,
   retornarMaior,
   maiorQueMedia,
   potencia_2,
   potencia_4,
   xor,
   raizDelta,
   xMaior,
   xMenor,
   somaInclusiva,
   somaExclusiva,
   multiplos,
   multComSoma,
   mod2,
   calcSequencia,
   fact,
   arranjo,
   maiorElemento,
   consultarDicionario,
   converterListaDigitos,
   del_posicao_n,
   inserir_posicao_x,
   returnIndex,
   quicksort,
   mergeSortedList,
   verificaLista,
   intersecaoLista,
   checar,
   encode,
   charToInt,
   decode
) where

-- 1
qtdIguais :: Int -> Int -> Int -> Int
qtdIguais a b c
    | a == b && a == c = 3
    | a == b || b == c || a == c = 2
    | otherwise = 0 

-- 2
calcularMedia :: Fractional a => a -> a -> a -> a
calcularMedia a b c = (a + b + c) / 3

retornarMaior :: (Ord a, Num b) => a -> a -> b
retornarMaior x y = if x > y then 1 else 0

maiorQueMedia :: (Fractional a, Ord a, Num b)  => a -> a -> a -> b
maiorQueMedia a b c = retornarMaior a media + retornarMaior b media + retornarMaior c media
                    where
                        media = calcularMedia a b c

-- 3
potencia_2 :: Num a => a -> a
potencia_2 x = x * x

-- 4
potencia_4 :: Num a => a -> a
potencia_4 x = potencia_2 x * potencia_2 x

-- 5
xor :: Bool -> Bool -> Bool
xor a b = a /= b -- if a == b then False 

-- 6
raizDelta :: Floating a => a -> a -> a -> a
raizDelta a b c = sqrt $ b ** 2 - 4 * a * c

xMaior :: Floating a => a -> a -> a -> a
xMaior a b c = ((-b) + delta) / (2 * a)
             where
                delta = raizDelta a b c

xMenor :: Floating a => a -> a -> a -> a
xMenor a b c = ((-b) - delta) / (2 * a)
             where
                delta = raizDelta a b c

-- 7
somaInclusiva :: Int -> Int -> Int
somaInclusiva a b
    | a < b = sum [a..b]
    | otherwise = sum [b..a]

somaExclusiva :: Int -> Int -> Int
somaExclusiva a b
    | a == b = 0
    | otherwise = somaInclusiva a b - a -b

-- 8
multiplos :: Int -> Int -> Int -> [Int]
multiplos x y z = filter (\a -> a `mod` z == 0) [x .. y] 

-- 9
multComSoma :: Int -> Int -> Int
multComSoma a b
    | a < 0 && b < 0 = sum (replicate (abs a) (abs b))
    | a < 0 = sum (replicate b a)
    | otherwise = sum (replicate a b)

-- 10
mod2 :: Int -> Int -> Int
mod2 x y = if x >= y then mod2 (x - y) y else x

-- 11
calcSequencia :: Floating a => Int -> a
calcSequencia n
    | n == 1 = sqrt 6
    | otherwise = sqrt (6 + calcSequencia (n - 1))

-- 12
fact :: Int -> Int
fact x = product [1..x]

arranjo :: Int -> Int -> Int
arranjo m n = (fact m) `div` (fact (m-n))

-- 13
posicaoMaior :: (Eq a, Num b) => a -> [a] -> b
posicaoMaior p (x:xs)
    | p == x = 0
    | otherwise = 1 + posicaoMaior p xs

maiorElemento :: Ord a => [a] -> (a, Int)
maiorElemento lista = (maior, posicaoMaior maior lista)
    where
        maior = maximum lista

-- 14
dic_10 :: [(Int, String)]
dic_10 = [(0,"zero"), (1,"um"), (2,"dois"), (3, "três"), (4,"quatro"), (5,"cinco"),  (6,"seis"), (7,"sete"), (8,"oito"), (9,"nove")]

consultarDicionario :: Int -> String
consultarDicionario i = head [snd x | x <- dic_10, fst x == i]

converterListaDigitos :: [Int] -> [String]
converterListaDigitos lista = map (consultarDicionario) lista 

-- 15
del_posicao_n :: [Int] -> Int -> [Int]
del_posicao_n (x:xs) n
    | n == 0 = xs
    | otherwise = [x] ++ del_posicao_n xs (n-1)

-- 16
inserir_posicao_x :: [Int] -> Int -> Int -> [Int]
inserir_posicao_x (x:xs) p n
    | p == 0 = [n] ++ [x] ++ xs
    | otherwise = [x] ++ inserir_posicao_x xs (p-1) n

-- 17
returnIndex :: Int -> [Int] -> Int
returnIndex p (x:xs)
    | p == 0 = x
    | otherwise = returnIndex (p-1) xs

-- 18
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort maiores
                where
                    menores = [a | a <- xs, a <= x]
                    maiores = [b | b <- xs, b > x]

mergeSortedList :: [Int] -> [Int] -> [Int]
mergeSortedList a b = quicksort (a ++ b)

-- 19
verificaLista :: Int -> [Int] -> Bool
verificaLista n (x:xs)
    | n == x = True
    | length xs == 0 = False
    | otherwise = verificaLista n xs

-- It's necessary to use "_ []" in case the last element of L2 is contained in L1. In that case,
-- the function will be executed again, therefore, will try to split an empty list.
intersecaoLista :: [Int] -> [Int] -> [Int]
intersecaoLista _ [] = []
intersecaoLista listaA (x:xs)
    | verificaLista x listaA == True = [x] ++ intersecaoLista listaA xs
    | length xs == 0 = []
    | otherwise = intersecaoLista listaA xs

-- 20
checar :: String -> Int -> Int
checar (x:xs) aux
    | length xs == 0 = aux
    | x == head xs = checar xs aux+1
    | x /= head xs = aux

encode :: String -> String
encode [] = []
encode (x:xs)
    | valorChecagem > 3 = "!" ++ show valorChecagem ++ [x] ++ encode (drop (valorChecagem-1) xs)
    | valorChecagem <= 3 = [x] ++ encode xs
    | length xs == 0 = []
    where
        valorChecagem = checar([x] ++ xs) 1

-- 21
charToInt :: Char -> Int
charToInt x = (read ([x]) :: Int)

decode :: String -> String
decode [] = []
decode (x:xs)
    | x == '!' = replicate (charToInt (xs !! 0)) (xs !! 1) ++ decode (drop 2 xs)
    | length xs == 0 = [x]
    | otherwise = [x] ++ decode xs
