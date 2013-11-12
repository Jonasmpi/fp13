module Muster3 where

-- gegeben in Aufgabenstellung
import PowerSeries
import Generator

-- imports
import Data.List
import Data.Function (on)
import Test.QuickCheck

-- ----------------------------------------------
-- Aufgabe 1
-- ----------------------------------------------
flip31 f x y z = f y x z
-- usw.

-- ----------------------------------------------
-- Aufgabe 2
-- ----------------------------------------------
power1a, power1b, power1b', power2 :: Integer -> Int -> Integer
power1a x 0 = 1
power1a x k | k < 0 = error "meep"
power1a x k = x * power1a x (k-1)

power1b x 0 = 1
power1b x k | k < 0 = error "meep"
power1b x k = foldl (*) 1 $ replicate k x

-- Idee: power1b' = foldl (*) 1 . flip replicate

power1b' x = product . flip replicate x
power2 = undefined -- Quadrierung habe alle hingekriegt

-- quickcheck!
check_power1 x k | k > 0 && k < 100000 = (power1a x k) == (power1b x k)
check_power1 _ _ = True
check_power2 x k | k > 0 && k < 100000 = (power1b x k) == (power1b' x k)
check_power2 _ _ = True


-- ----------------------------------------------
-- Aufgabe 3
-- ----------------------------------------------

-- Generalisierung
sterling n k z = undefined -- *faulenz*
sterling1 n k = sterling n k n
sterling2 n k = sterling n k k


-- ----------------------------------------------
-- Aufgabe 4
-- ----------------------------------------------

-- dark magic
default (Integer, Rational, Double)

sinx = integral cosx 
cosx = 1 - (integral sinx)

-- testing the sin-cos identity
check_sin n | n>0 && n<300  = let cs = take n $ sqrt $ 1 - sinx^2
                         in cs == take n cosx
            | otherwise  = True

check_deriv n | n>0 && n<500 
                  = (take n (sinx)) == (take n (deriv $ integral sinx))
              | otherwise = True

check_fastint n | n>0 && n<1000 
                  = (take n (sinx)) == (take n (deriv $ fastint sinx))
              | otherwise = True


-- ----------------------------------------------
-- Aufgabe 5
-- ----------------------------------------------
type Data = (Int, String)
maxRank, maxRank', maxRank'' :: [Data] -> String
maxRank' = snd . maximumBy (compare `on` fst)

-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldl1 :: (a -> a -> a) -> [a] -> a
-- foldl mit mindestens einem Element in der Liste

maxRank xs = snd $ foldl1 f xs
    where f (a, b) (c, d) | a > c = (a, b)
                          | otherwise = (c, d)

maxRank'' = snd . foldl1 f
    where f x@(a, b) y@(c, d) | a > c = x
                              | otherwise = y

testRank :: ([Data] -> String) -> String
testRank f = f $ kvGen 20 200

check_rank1 xs = (maxRank xs) == (maxRank' xs)
check_rank2 xs = (maxRank' xs) == (maxRank'' xs)

{-
-- overkill, abuse of notation, Data should be a proper type
instance Ord Data where
    compare (a, b) (c, d) = compare a c

maxRank''' = snd . maximum
-}