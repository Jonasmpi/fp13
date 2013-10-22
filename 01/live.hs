
import Test.QuickCheck

------------------------------------------------------
-- Aufgabe 1
------------------------------------------------------

fib1, fib2 :: Int -> Int
fib1 = undefined -- TODO!
fib2 = undefined

test_equal n = (fib1 n) == (fib2 n)

test_equal_abs n = (fib1 $ abs n) == (fib2 $ abs n)

------------------------------------------------------
-- Aufgabe 3
------------------------------------------------------

data NoDeriv = N | M | O | P

data Deriv1 = H | J | K | L
              deriving Show

foo = H
bar = print foo      -- works

foo' = N
-- bar' = print foo' -- fails

data Deriv2 = D | E | F
              deriving Eq

biz = D == E     -- works
-- biz' = N == M -- fails

-- alltogether
data Deriv3 = A | B | C | X
              deriving (Eq, Show)

boz = if C == C 
      then print A
      else print X


------------------------------------------------------
-- Aufgabe 4
------------------------------------------------------

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

bizz [] = 0
bizz [x] = 1
bizz [x,y] = 2
bizz xs = -42

isEmptyStupid xs = 0 == length xs

