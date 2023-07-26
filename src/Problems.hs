{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Problems (
    myLast,
    myButLast,
    elementAt,
    myLength,
    myReverse,
    isPalindrome,
    check,
    compress,
    pack,
    encode,
    dupli,
    repli,
    dropEvery,
    split',
    slice,
    removeAt
) where

-- P1 [1, 2, 3, 4]
myLast :: Eq a => [a] -> a
myLast [] = error "Lista vazia."
myLast (x:xs)
    | null xs = x
    | otherwise = myLast xs

-- P2
myButLast :: Eq a => [a] -> a
myButLast [] = error "Lista vazia."
myButLast (_:xs)
    | length xs == 2 = head xs
    | otherwise = myButLast xs

-- P3
elementAt :: Eq a => [a] -> Int -> a
elementAt [] _ = error "Lista vazia."
elementAt (x:xs) k
    | k-1 == 0 = x
    | otherwise = elementAt xs (k-1)

-- P4 [1, 2, 3]
myLength :: [a] -> Int
myLength list = sum [1 | _ <- list, otherwise]

-- P5 [1, 2, 3] -> [3, 2, 1]
myReverse :: Eq a => [a] -> [a]
myReverse list
    | length list == 1 = [head list]
    | otherwise = myLast list : myReverse (init list)

-- P6 [1, 2, 3, 4] -> [1, 2] [3, 4] a na 
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == myReverse list

-- P8
check :: Eq a => [a] -> Int
check (x:xs)
    | null xs = 1
    | x == head xs = 1 + check xs
    | otherwise = 1

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs)
    | qtd >= 2 = x : compress (drop (qtd - 1) xs)
    | qtd < 2 = x : compress xs
    | null xs = []
    where
        qtd = check (x : xs)

-- P9
pack :: Eq a => [a] -> [[a]]
pack list
    | null list = []
    | qtd > 0 = replicate qtd (head list) : pack (drop qtd list)
    where
        qtd = check list

-- P10 
encode :: Eq a => [a] -> [(Int, a)]
encode list = zip numbers letters
    where
        numbers = map myLength (pack list)
        letters = map head (pack list)

-- P14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- P15
repli :: [a] -> Int -> [a]
repli list n = concat (map (replicate n) list)

-- P16
dropEvery :: [a] -> Int -> [a]
dropEvery (x:xs) n
    | null xs = [x]
    | n == 1 = dropEvery xs 3
    | otherwise = x : dropEvery xs (n-1)

-- P17
split' :: [a] -> Int -> ([a], [a])
split' list n = (take n list, drop n list)

-- P18
slice :: [a] -> Int -> Int -> [a]
slice list x y = take (y-x+1) (drop (x-1) list)

-- 

-- P20
removeAt :: Eq a => Int -> [a] -> (a, [a])
removeAt i list = (element, filter (/=element) list)
    where
        element = list !! (i-1)
