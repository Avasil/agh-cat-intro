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

-- Take a look at http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Maybe.html
-- If you're looking for more functions that work on Maybe


-- !! EXERCISES !! --
-- 1.
-- write a total maybeSqrt function that takes a 'Double' and returns 'Maybe Double'
-- 
-- Partial function for reference:
-- sqrt :: Double -> Double
-- sqrt x
--     | x >= 0 = sqrt x
--     | otherwise = error "Square root of negative number"