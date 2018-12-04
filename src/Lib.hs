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

test :: IO ()
test = putStrLn "Hello, SMaDA!"