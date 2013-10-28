-*- Literate Haskell -*-

Musterlösung Blatt 1.
\begin{code}
module Main where
\end{code}

Here go our imports.

\begin{code}
import System.Environment (getArgs)
import Test.QuickCheck
import Prelude
\end{code}


Aufgabe 1.

\begin{code}
fib1 :: Int -> Int
fib1 1 = 1
fib1 2 = 1
fib1 n | n > 0 = fib1 (n-1) + fib1 (n-2)
fib1 _ = error "non-positive input"

fib2 n | n > 0 =
  let phi = (1.0 + (sqrt 5)) / 2.0
      psi = 1.0 - phi
      res = (phi^n - psi^n) / (phi - psi)
  in round res 
fib2 _ = error "non-positive input"
\end{code}

Von Valentin Ochs.
\begin{code}
-- Matrizenmultiplikation
fib3 :: Int -> Int
fib3 n | n < 0 = -(-1)^(-n)*(p!!1)
       | otherwise = p!!1
  where
      x = [1,1,1,0]
      p = mP x (abs n)
      mP [a,b,c,d] e | e == 0    = [1,0,0,1]
                     | e == 1    = [a,b,c,d]
                     | otherwise = mM [a,b,c,d] $ mP [a,b,c,d] $ e-1
      mM [a,b,c,d] [e,f,g,h] = [a*e+b*g, a*f+b*h, c*e+d*g, c*f+d*h]
\end{code}

Quickcheck dazu.
\begin{code}
prop_fib n | n>0 && n<40 = (fib1 n) == (fib2 n)
           | otherwise = True
-- the test makes no sense for non-positive vals and fib1 takes insanely long time for large inputs

-- unerwartetes Ergebnis...
prop_fib2 n | n>0 && n<400 = (fib2 n) == (fib3 n)
            | otherwise = True
\end{code}

Aufgabe 2.

\begin{code}
data Boolean = Treacherous | Faithful
             deriving (Show, Eq)
\end{code}

Eine Alternative wäre:
\begin{code}
type MyBoolean = Bool
\end{code}

Die, die noch and, or, not definiert haben, waren gut. ;)

Aufgabe 3.

\begin{code}
data Dna = G | C | A | T
         deriving (Eq, Show)

dna2int :: Dna -> Int
dna2int G = 1
dna2int C = 3
dna2int A = 7
dna2int T = 8

int2dna :: Int -> Maybe Dna
int2dna 1 = Just G
int2dna 3 = Just C
int2dna 7 = Just A
int2dna 8 = Just T
int2dna _ = Nothing
\end{code}

Quickcheck Properties dazu (nicht Bestandteil der Lösung!)
\begin{code}
prop_dna2 i | prelims && d /= Nothing = i == res
  where prelims = i == 1 || i == 3 || i == 7 || i == 8
        d = int2dna i
        Just d' = d -- this is also pattern matching
        res = dna2int d'
prop_dna2 _ = True -- ignore other inputs
-- this is an ugly hack, better way: own generator for test values
\end{code}

Aufgabe 4.

\begin{code}
match2 [_, _] = True
match2 _      = False
\end{code}

Das funktioniert aus mit unendlichen listen:

match2 [1..] == False

Quickcheck!
\begin{code}
prop_match xs = lhs == rhs
  where lhs = match2 xs
        rhs = 2 == length xs
\end{code}

The main function will be called upon start of the executable. A bit too complicated for starters, but produces a nice output.

\begin{code}
main = do
  args <- getArgs
  let x :: Int
      x = read (head args)
      res = fib2 x
  putStr $ (show x) ++ "'th Fibonacci number is: "
  print  $ fib2 x
  putStr $(show x) ++ " corresponds to Dna value "
  print  $ int2dna x
  putStrLn "done"
\end{code}

