-*- Literate Haskell -*-

Literate programming inverts the comments and the program text. It's the program text that needs to be specially marked. There is another, more TeX-similar formatting style for literate programming in Haskell.

\begin{code}
module Main where
\end{code}

This is a comment again.

Here go out imports.

\begin{code}
import System.Environment (getArgs)
import Prelude -- old-style comments like this still work
\end{code}

the actual working function maps an int to an int with pattern matching

\begin{code}
foo :: Int -> Int
foo 42 = -1
foo _  = 0
\end{code}

the main function will be called upon start of the executable.

do notation and <- binding in the main are black magic for now, disregard them.
specifics of let here are also black magic, the "standard" syntax is like this:

f x = let y = 42
      in x*y

the essence of the `main`: we apply the working function `foo` to it and print the result. The parameter `x` is fetched from the command line

\begin{code}
main = do
  args <- getArgs
  let x = read $ head args
      res = foo x
  print res
\end{code}

Note the 2D syntax at:

  let x = head args
      res = foo x

We are done here!
