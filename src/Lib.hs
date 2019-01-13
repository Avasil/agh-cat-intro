module Lib
    ( test
    ) where

import Data.Char

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

multIfEven :: [Int] -> [Int]
multIfEven [] = []
multIfEven (h:t) = f h : multIfEven t
    where f h = if even h then h * 2 else h

-- Should work like this:
-- multIfEven [1, 2, 3, 4]
-- [1, 4, 3, 8]

myFmap :: (a -> b) -> [a] -> [b]
myFmap _ [] = []
myFmap f (h:t) = f h : myFmap f t

-- Exercise 3:
-- (1) Implement a function that removes capitalized letters from string (without using filter)
-- (2) Implement your own version of filter.
-- (3) Implement (1) in terms of (2)

-- Should work like this:
-- removeCapitalized "Grzybów było w bród."
-- "rzybów było w bród."

removeCapitalized :: String -> String
removeCapitalized [] = []
removeCapitalized (h:t) = f h ++ removeCapitalized t
    where f x = if isUpper x then [] else [x]

removeCapitalized' :: String -> String
removeCapitalized' [] = []
removeCapitalized' (h:t)
    | isUpper h = removeCapitalized' t
    | otherwise = h : removeCapitalized' t

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (h:t)
    | f h = h : filter' f t
    | otherwise = filter' f t

-- Exercise 4:
-- (1) Implement a function that sums all elements in the list (without using fold)
-- (2) Implement a function that concatenates a list of lists into single list (without using fold)
-- (3) Implement fold on your own
-- (4) Implement (1) and (2) in terms of (3)

-- Should work like this:
-- mySum [1, 2, 3, 4, 5]
-- 15
-- myConcat [[1], [2], [3]]
-- [1,2,3]

test :: IO ()
test = putStrLn "Hello, SMaDA!"