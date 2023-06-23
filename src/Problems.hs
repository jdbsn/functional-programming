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
    encode
) where

-- P1 [1, 2, 3, 4]
myLast :: Eq a => [a] -> a
myLast [] = error "Lista vazia."
myLast (x:xs)
    | length xs == 0 = x
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
myLength :: Eq a => [a] -> Int
myLength list = sum [1 | _ <- list, otherwise]

-- P5 [1, 2, 3] -> [3, 2, 1]
myReverse :: Eq a => [a] -> [a]
myReverse list
    | length list == 1 = [(head list)]
    | otherwise = [myLast list] ++ myReverse (init list)

-- P6 [1, 2, 3, 4] -> [1, 2] [3, 4] a na 
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == myReverse list

-- P8 aaaa
check :: Eq a => [a] -> Int
check (x:xs)
    | length xs == 0 = 1
    | x == head xs = 1 + check xs 
    | otherwise = 1

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs)
    | qtd >= 2 = [x] ++ compress (drop (qtd - 1) xs)
    | qtd < 2 = [x] ++ compress xs
    | length xs == 0 = []
    where
        qtd = check([x] ++ xs)

-- P9
pack :: Eq a => [a] -> [[a]] -- aaaabaa -- a
pack list
    | length list == 0 = []
    | qtd > 0 = [replicate qtd (head list)] ++ pack (drop qtd list)
    where
        qtd = check list

-- P10 
encode :: Eq a => [a] -> [(Int, a)]
encode list = zip numbers letters
    where
        numbers = map myLength (pack list)
        letters = map head (pack list)
