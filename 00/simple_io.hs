module Main where

-- this is a comment

import System.Environment (getArgs) -- we have fetched the fu'n getArgs
                                    -- from the module System.Environment

import Prelude

-- the actual working function
foo :: Int -> Int
foo 42 = -1
foo _  = 0

-- the main function, it will be called upon start of the executable
main = do
  args <- getArgs          -- do notation and <- binding are black magic for now
  let x = read (head args) -- specifics of let here are also black magic
  let res = foo x          -- apply the actual function
  print res                -- output the result

