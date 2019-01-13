module Lab2 where

import Control.Monad (mfilter)
import Data.Maybe

divide :: Double -> Double -> Double
divide _ 0 = error "Division by 0"
divide x y = x / y

divideM :: Double -> Double -> Maybe Double
divideM _ 0 = Nothing
divideM x y = Just (x / y)

-- instance  Functor Maybe  where
--     fmap _ Nothing       = Nothing
--     fmap f (Just a)      = Just (f a)
--
-- fmap maps a function over "container" so in this case it adds + 10 to contents of the Maybe
-- *Lab2> fmap (+10) (divideM 5 2)
-- Just 12.5
--
-- "do notation" lets us temporarily extract value from the structure
-- "$" is just a syntax sugar for parentheses:)
-- "return" promotes plain value to proper structure (in our case Maybe)
-- 
-- *Lab2> do a <- divideM 5 2; b <- divideM 6 2; return $ a + b
-- Just 5.5
-- *Lab2> do a <- divideM 5 0; b <- divideM 6 2; return $ a + b
-- Nothing

addM :: Maybe Double -> Maybe Double -> Maybe Double
addM (Just x) (Just y) = Just (x + y)
addM _        _        = Nothing

-- *Lab2> mfilter (> 10) (Just 2)
-- Nothing
-- *Lab2> mfilter (> 10) (Just 12)
-- Just 12
-- *Lab2> fromMaybe 0.0 (Just 10)
-- 10.0


--                                           !! EXERCISES !!

-- Exercise 1. (Algebraic Data Types + Maybe)
-- a) Define your own type (`data` keyword) called `Chocolate Cake` 
--    which can be either `Molten`, `Fudge`, `Red Velvet` or `Ding Dong`

data ChocolateCake = Molten | Fudge | RedVelvet | DingDong deriving Show
-- b) Write a function `getCake` that accepts a `String` and converts it to `Chocolate Cake`

getCake :: String -> Maybe ChocolateCake
getCake "Molten"     = Just Molten
getCake "Fudge"      = Just Fudge
getCake "Red Velvet" = Just RedVelvet
getCake "Ding Dong"  = Just DingDong
getCake _            = Nothing

-- c) Test b) with following inputs:
--    - getCake "Molten"
--    - getCake "Apple Pie"
--    - getCake "grt8w3gts35gozusw43w4g"
-- Make sure it doesn't throw any exceptions!

orders :: [String]
orders = ["Molten", "Fudge", "Churros", "Red Velvet", "Molten", "Ding Dong", "Bacon"]
-- d) Take a list of orders defined above and write a function that transforms it to [Chocolate Cake], discarding wrong orders. 
-- Take a look at http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Maybe.html for useful functions

d :: [ChocolateCake]
d = mapMaybe getCake orders





-- Exercise 2. Write a total maybeSqrt function that takes a 'Double' and returns 'Maybe Double'
-- 
-- Partial function for reference:
-- sqrt :: Double -> Double
-- sqrt x
--     | x >= 0 = sqrt x
--     | otherwise = error "Square root of negative number"

maybeSqrt :: Double -> Maybe Double
maybeSqrt x
    | x >= 0    = Just $ sqrt x
    | otherwise = Nothing

-- Exercise 3. Use `maybeSqrt`, `divideM` with `fmap` / `do notation` to write functions 
--             computing following equations:
--   a) sqrt(x) * 5
--   b) sqrt(x) + sqrt(y)
--   c) sqrt(sqrt(x) / sqrt(y))





-- It is probably cumbersome to write (Maybe is overkill for math equations)
-- but it should get you more comfortable with using fmap/do notation.
-- 
-- Often the better way is to just validate the input once 
-- and then use more convenient functions. Like this:
--
-- a :: Double -> Maybe Double
-- a x
--    | x >= 0    = Just $ sqrt x * 5
--    | otherwise = Nothing
--
-- Exercise 3d. Now try to write 3c) in this manner!





-- Exercise 4. Try writing `map2` function that combines two `Maybe` values using
--             provided function. If either `Maybe` value is `Nothing` then return `Nothing`.
-- TIP: signature should be map2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- TIP2: Remember that `do notation` and `fmap` both have specified behavior built-in
-- 
-- You can call your function like this: `map2 (Just 4) (Just 4) (+)`
-- Or even `map2 (+) (sqrt 5) (sqrt 7)` to solve exercise 3b!