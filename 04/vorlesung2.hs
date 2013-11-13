{-# OPTIONS_GHC -XFlexibleInstances #-}
module Vorlesung2 where

import Data.List
import Test.QuickCheck
import Control.Monad ((>=>))

------------------------------------------------------------
-- zur Erinnerung: Listen!
-- 
-- viel Doku: http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-List.html

listInts = iterate (+1) 0 -- == [1..]

boringList = repeat 'a'

boringFiniteList = take 10 boringList
boringFiniteList' = replicate 10 'a'

-- gibt's auch: unfoldr, Faltung rückgängig machen
-- unfoldr :: (b -> Maybe (b, a)) -> b -> [a]
under n | n < 10 = Just (n, n+1)
        | otherwise = Nothing
underTen = unfoldr under 1

-- intersperse und Ko
-- String ist ja [Char]
string = "Mississipi"
snakeString = intersperse 's' string
uniqueString = nub string

------------------------------------------------------------

-- const läßt den 2. Argument aus:
const' x _ = x
prop_const1 a b = a == const a b
prop_const2 a b = (const a b) == (const' a b)

------------------------------------------------------------

-- Monaden! Funktoren!
-- Either String a ist auch eine Monade, ähnlich zu Maybe
-- Code ist schon in Data.Either drin, daher auskommentiert
{-
instance Monad (Either String) where
    Left s >>= _ = Left s
    Right v >>= f = f v
    
    return v = Right v
    fail s = Left s

instance Functor (Either String) where
    fmap _ (Left v) = Left v
    fmap f (Right v) = Right $ f v
-}
-- gelten hier die monadischen Gesetze und die Funktor-Gesetze?

{-
-- Monad laws:
return a >>= k  ==  k a
m >>= return    ==  m
m >>= (\x -> k x >>= h) == (m >>= k) >>= h

-- Monad laws, with Kleisli operator:
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) is the Kleisli composition

return >=> g == g
f >=> return == f
(f >=> g) >=> h == f >=> (g >=> h)

-- Functor laws
fmap id  ==  id
fmap (f . g) == fmap f . fmap g

-- Functor and Monad law:
fmap f xs == xs >>= return . f

-- Basic reference: http://hackage.haskell.org/package/base-4.6.0.1/docs/src/Control-Monad.html
-}


------------------------------------------------------------
-- Again code of SimpleState, see also

type SimpleState s a = s -> (a, s)
return' a s = (a, s)
bind :: SimpleState s a 
     -> (a -> SimpleState s b)
     -> SimpleState s b
bind step makeStep oldState
  = let (result, newState) = step oldState 
        -- run step with our initial state oldState.
    in (makeStep result) newState
    -- denkt dran: step ist eine Funktion!

{-
-- basic idea:
instance Monad (SimpleState s) where
    return = return'
    (>>=) = bind
-}
