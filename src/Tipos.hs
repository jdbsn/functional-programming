module Tipos (
    subtracao,
    buscar
) where

data Nat = Zero | Suc Nat deriving Show

subtracao :: Nat -> Nat -> Maybe Nat
subtracao Zero Zero = Just Zero
subtracao Zero _ = Nothing
subtracao m Zero = Just m
subtracao (Suc m) (Suc n) = subtracao m n

type Assoc k v = [(k,v)]

buscar :: (Eq a, Eq b) => a -> Assoc a b -> Maybe b
buscar k xs
    | xs == [] = Nothing
    | k == fst (head xs) = Just (snd (head xs))
    | otherwise = buscar k (tail xs)