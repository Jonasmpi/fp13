-*- Literate Haskell -*-

Literate programming inverts the comments and the program text. It's the program text that needs to be specially marked. Like this:

> module Main where

This is a comment again.

> import System.Environment (getArgs)

we have fetched the fu'n getArgs from the module System.Environment

> import Prelude -- we can also have old-style comments like this

the actual working function

> foo :: Int -> Int

it maps an int to an int with pattern matching

> foo 42 = -1
> foo _  = 0

the main function, it will be called upon start of the executable

> main = do
>  args <- getArgs

do notation and <- binding are black magic for now, disregard them.
specifics of let here are also black magic, the "standard" syntax is like this:

f x = let y = 42
      in x*y


continue with the main! x here fetches the first argument...

>  let x = read $ head args

we apply the working function `foo` to it and print the result:

>  let res = foo x
>  print res

Done!