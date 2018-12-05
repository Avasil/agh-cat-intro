module Lib
    ( test
    ) where

-- Recursion Example

data Tree a = Empty | Node (Tree a) a (Tree a)

height :: Tree a -> Int
height Empty = 0
height (Node left _ right) = 1 + max (height left) (height right)

-- Exercise 1: 
-- Implement recursive fibonacci function three times
-- using different type of pattern matching each time


-- Exercise 2:
-- (1) Implement a function that multiplies each element of the list if it is even
-- (2) Implement your own version of fmap. Hint: Generalize your first function
-- (3) Implement (1) in terms of (2)

-- Should work like this:
-- multIfEven [1, 2, 3, 4]
-- [1, 4, 3, 8]

test :: IO ()
test = putStrLn "Hello, SMaDA!"