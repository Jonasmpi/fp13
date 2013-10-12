-*- Literate Haskell -*-

\begin{code}
module Main where
\end{code}

Here go our imports.

\begin{code}
import System.Environment (getArgs)
import Test.QuickCheck
import Prelude
\end{code}

The actual working function maps an int to an int with pattern matching

\begin{code}
foo :: Int -> Int
foo 42 = -1
foo _  = 0
\end{code}

The quickcheck properties. Here they are stupid, but you can change them to be more meaningful later. A property `prop_foo` can be tested in GHCi with `quickCheck prop_foo`:

*Main> quickCheck prop_isInt 
Loading package array-0.4.0.1 ... linking ... done.
Loading package deepseq-1.3.0.1 ... linking ... done.
Loading package old-locale-1.0.0.5 ... linking ... done.
Loading package time-1.4.0.1 ... linking ... done.
Loading package random-1.0.1.1 ... linking ... done.
Loading package containers-0.5.0.0 ... linking ... done.
Loading package pretty-1.1.1.0 ... linking ... done.
Loading package template-haskell ... linking ... done.
Loading package QuickCheck-2.6 ... linking ... done.
+++ OK, passed 100 tests.
*Main> quickCheck prop_foo 
+++ OK, passed 100 tests.

\begin{code}
prop_isInt n = n+1+1 == n+2

prop_foo n | n == 42 
               = foo n == -1
           | otherwise 
               = foo n == 0
\end{code}

The main function will be called upon start of the executable.

\begin{code}
main = do
  args <- getArgs
  let x = read $ head args
      res = foo x
  print res
\end{code}

We are done here!
