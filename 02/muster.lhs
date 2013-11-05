-*- Literate Haskell -*-

Übungsblatt 2.

\begin{code}
module Main where
\end{code}

\begin{code}
import System.Environment (getArgs)
import Test.QuickCheck
import Prelude

import Data.Maybe
\end{code}

Aufgabe 1.

\begin{code}
head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

tail' :: [a] -> Maybe [a]
tail' [] = Nothing
tail' (x:xs) = Just xs

-- an alternative
tail'' :: [a] -> [a]
tail'' [] = []
tail'' xs = tail xs -- haha!

init' :: [a] -> Maybe [a]
init' [] = Nothing
init' xs = Just (innerInit xs) 
  where innerInit [] = []
        innerInit [x] = []
        innerInit (x:xs) = x:innerInit xs
        

-- an alternative
init'' :: [a] -> [a]
init'' [] = []
init'' xs = init xs -- haha!

last' :: [a] -> Maybe a
last' [] = Nothing
-- last' xs = Just (last xs) -- haha, let's do it ourselves
last' xs = Just (innerLast xs)
  where innerLast [x] = x
        innerLast (x:xs) = innerLast xs
        innerLast _ = error "unexpected empty list"
\end{code}

The quickcheck properties.

\begin{code}
prop_head xs | xs == [] = Nothing == head' xs
             | otherwise = (head' xs) == (Just $ head xs)

prop_tail xs | xs == [] = Nothing == tail' xs
             | otherwise = (tail' xs) == (Just $ tail xs)

prop_tail2 xs | xs == [] = [] == tail'' xs
              | otherwise = (tail'' xs) == (tail xs)

prop_init xs | xs == [] = Nothing == init' xs
             | otherwise = (init' xs) == (Just $ init xs)

prop_init2 xs | xs == [] = [] == init'' xs
              | otherwise = (init'' xs) == (init xs)

prop_last xs | xs == [] = Nothing == last' xs
             | otherwise = (last' xs) == (Just $ last xs)
\end{code}

The output:
*Main> quickCheck prop_head 
+++ OK, passed 100 tests.
*Main> quickCheck prop_tail
+++ OK, passed 100 tests.
*Main> quickCheck prop_tail2
+++ OK, passed 100 tests.
*Main> quickCheck prop_init
+++ OK, passed 100 tests.
*Main> quickCheck prop_init2
+++ OK, passed 100 tests.
*Main> quickCheck prop_last 
+++ OK, passed 100 tests.


Aufgabe 2.

Idee: Listen sind auch sowas wie Maybe! Wert x passt ~> [x], passt nicht ~> []. Das ist eine map. Dann diese Liste von Listen konkatenieren, was nach Aufgabenstellung foldr ist.

\begin{code}
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs
  = let g x | f x = [x]
            | otherwise = []
    in concat (map g xs)
\end{code}

Ähnlich, mit Maybies. Es gibt in Data.Maybe eine Funktion

catMaybes :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

die einfach Nothings 'rausschmeißt. Dieselbe Idee, aber sauberer. Aber eigentlich ohne fold.

\begin{code}
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f xs
  = let g x | f x = Just x
            | otherwise = Nothing
    in catMaybes $ map g xs
\end{code}

... und nochmal anders! List comprehensions!
\begin{code}
filter''' f xs = [ x | x<- xs, f x ]
\end{code}

Quickcheck it.

\begin{code}
testF x = x `mod` 3 == 0 -- not very nice, but ok
prop_filter1 xs = (filter testF xs) == (filter' testF xs)
prop_filter2 xs = (filter testF xs) == (filter'' testF xs)
prop_filter3 xs = (filter testF xs) == (filter''' testF xs)
\end{code}

*Main> quickCheck prop_filter1
+++ OK, passed 100 tests.
*Main> quickCheck prop_filter2
+++ OK, passed 100 tests.
*Main> quickCheck prop_filter3
+++ OK, passed 100 tests.


Aufgabe 3.

\begin{code}
addUltimateAnswer :: [Int] -> [Int]
addUltimateAnswer [] = []
addUltimateAnswer (x:xs) = (x+42):addUltimateAnswer xs

addUltimateAnswer' :: [Int] -> [Int]
addUltimateAnswer' = map (+42) -- currying!
\end{code}


Aufgabe 4.

\begin{code}
similarPairDirect :: (Int, Int) -> (Int, Int) -> Bool
similarPairDirect (a, b) (x, y) = (a == y && b == x) || (a == x && b == y)

-- a "nicer" solution
similarPair :: (Int, Int) -> (Int, Int) -> Bool
similarPair left right = left == right || left == (rot right)
  where rot (x, y) = (y, x) -- could have also used a library function
\end{code}

Quickcheck this!
\begin{code}
prop_simpair x y = (similarPairDirect x y) == (similarPair x y)
\end{code}

*Main> quickCheck prop_simpair 
+++ OK, passed 100 tests.
*Main> similarPair (1,2) (1,2)
True
*Main> similarPair (1,2) (3,2)
False
*Main> similarPair (1,2) (2,1)
True



The main function will be called upon start of the executable.

\begin{code}
main = do
  args <- getArgs
  let [x, y, z, t] = map read (take 4 args)
      res = similarPair (x, y) (z, t)
  print res
\end{code}
