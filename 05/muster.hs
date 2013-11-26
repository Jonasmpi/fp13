module Musterloesung05 where

import Control.Monad
import Data.Functor

-- Aufgabe 1
data List a = Cons a (List a) | Nil

map' :: (a -> b) -> List a -> List b
map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

cat :: List a -> List a -> List a
cat xs Nil = xs
cat Nil ys = ys
cat (Cons x xs) ys = Cons x (cat xs ys)

concat' :: List (List a) -> List a
concat' Nil = Nil
concat' (Cons x xs) = cat x $ concat' xs

instance Monad List where
    xs >>= f = concat' $ map' f xs
    return x = Cons x Nil
    fail _   = Nil

instance Functor List where
    fmap = map'


-- Aufgabe 3
list2ones :: [a] -> [Int]
list2ones = map (\_ -> 1)

average = uncurry (/) . foldr (\x (s, l) -> (x+s, 1+l)) (0,0)

-- Aufgabe 4
pairAndSquare = map (\x -> (x, x^2))

numTwins, numTwins :: Eq a => [a] -> Int
numTwins xs = let input = zip xs $ tail xs 
                  pairs = filter (\(x, y) -> x==y) input
              in length pairs

numTwins' xs = length $ filter (uncurry (==)) $ zip xs $ tail xs

addElems = foldr1 (\(x, y) (s1, s2) -> (s1+x, s2+y))

ordPaare :: Ord a => [(a, a)] -> [(a, a)]
ordPaare = filter (uncurry (<=))
