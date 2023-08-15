--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Problems (
    ListItem(..),
    myLast,
    myButLast,
    elementAt,
    myLength,
    myReverse,
    isPalindrome,
    flatten,
    check,
    compress,
    pack,
    encode,
    encodeModified,
    aux,
    dupli,
    repli,
    dropEvery,
    split',
    slice,
    rotate,
    removeAt,
    insertAt,
    range,
    isPrime,
    primesR
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

-- P7
data NestedList a = Elem a | List [NestedList a] deriving Show

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

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

-- P11
data ListItem a = Single a | Multiple Int a deriving Show

aux :: (Int, a) -> ListItem a
aux l = if fst l == 1 then Single (snd l) else uncurry Multiple l

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified list = [aux x | x <- encode list]

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

-- P19
rotate :: [a] -> Int -> [a]
rotate list n = drop size list ++ take size list
    where
        size = if n > 0 then n else length list + n

-- P20
removeAt :: Eq a => Int -> [a] -> (a, [a])
removeAt i list = (element, filter (/=element) list)
    where
        element = list !! (i-1)

-- P21
insertAt :: a -> [a] -> Int -> [a]
insertAt e list p = take (p-1) list ++ [e] ++ drop (p-1) list

-- P22
range :: Enum a => a -> a -> [a]
range x y = [x..y]

-- P31
isPrime :: Int -> Bool
isPrime n = length [x | x <- [1..n], n `mod` x == 0] == 2

-- P39
primesR :: Int -> Int -> [Int]
primesR x y = filter isPrime [x..y]