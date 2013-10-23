module Vorlesung2 where

-- partial function
foo 1 2 = True
foo 3 4 = True
foo 7 _ = False
-- call `foo 42 1` and it fails
-- that's why:
foo _ _ = True
-- a catch-all clause

-- alternative to multiple equation:
-- guard expressions
foo' x | x==0      = 42
       | x<3       = 96
       | otherwise = -1

foo'' x | x==0      = 42 + y
        | y<3       = 96
        | otherwise = -1
        where y = round $ sin $ 2*pi*x -- some complicated function
-- note the where expression

foo''' x y = 
    let z = x*y
        t = x+y-1
    in x-z+t
-- let ... in is the same as where clause

-- f is tail-recursive
f x | terminate x = x
    | otherwise   = f (r x)
    where r = undefined -- r is some other function not calling f
          terminate = undefined -- terminate was a `a -> Bool` function
                                -- for the guard
-- tail-recursion can be efficiently implemented as a loop by a compiler
-- idea: need no call stack

-- a reimplementation of map
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x):(map f xs)

-- another implementation, very similar, might be better for understanding
otherMap :: (a -> b) -> [a] -> [b]
otherMap f [] = []
otherMap f xs = 
    let x    = head xs
        ts   = tail xs
        res  = f x
        call = map f ts
    in res:call
