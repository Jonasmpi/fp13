module IdMon where

import Control.Monad
import Test.QuickCheck

data Id a = Id a deriving (Show, Eq)

instance Monad Id where
  return = Id
  (Id x) >>= f = f x

test :: Int -> Id Int 
test x = (return x) >>= \x -> Id $ x+1

-- ein Paar Monad Laws
prop1 x     = (return x >>= Id)  ==  Id x
prop2 x     = (Id >>= return) x  ==  Id x

-- testen:
--  quickCheck prop1
--  quickCheck prop2

main = print $ test 41
