{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module Lab3 where

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import Data.IntMap.Strict
import qualified Data.IntMap.Strict as IntMap

-- Credits: https://github.com/snowleopard/alga
test :: Testable a => String -> a -> IO ()
test str p = do
    result <- quickCheckWithResult (stdArgs { chatty = False }) p
    if isSuccess result
        then putStrLn $ "OK: " ++ str
        else do
            putStrLn $ "\nTest failure:\n    " ++ str ++ "\n"
            putStrLn $ output result

-- class Semigroup m where
--     (<>) :: m -> m -> m
-- 
-- associativity: 
-- (a <> b) <> c == a <> (b <> c)

associativity :: (Eq a, Monoid a) => a -> a -> a -> Bool
associativity a b c = ((a <> b) <> c) == (a <> (b <> c))

-- class Semigroup m => Monoid m where
--     mempty :: m
--
-- left identity:  mempty <> a == a
-- right identity: a <> mempty == a

leftIdentity :: (Eq a, Monoid a) => a -> Bool
leftIdentity a = mempty <> a == a

rightIdentity :: (Eq a, Monoid a) => a -> Bool
rightIdentity a = a <> mempty == a

-- Bounded Join-Semilattice
class Monoid m => Semilattice m where
   join :: m -> m -> m
   join = (<>)

   minBound :: m
   minBound = mempty

-- commutativity:  a `join` b == b `join` a
-- idempotence:    a `join` a == a

commutativity :: (Eq a, Semilattice a) => a -> a -> Bool
commutativity a b = a `join` b == b `join` a

idempotency :: (Eq a, Semilattice a) => a -> Bool
idempotency a = a `join` a == a


-- functions on IntMap: 
-- http://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-IntMap-Strict.html

-- https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type#G-Counter_(Grow-only_Counter)
newtype GCounter = GCounter (IntMap Int) 
    deriving (Arbitrary, Eq, Show)

-- to increment the counter we need to increment counter on given replica's id
increment :: Int -> GCounter -> GCounter
increment id (GCounter imap) = GCounter (IntMap.insertWith (+) id 1 imap)

initial :: GCounter
initial = GCounter IntMap.empty

-- to merge we need to take max value of each node
merge :: GCounter -> GCounter -> GCounter
merge (GCounter x) (GCounter y) = GCounter $ IntMap.unionWith max x y

-- value of the total counter is the sum of all nodes
value :: GCounter -> Int
value (GCounter imap) = sum imap

splitBrain = do
       let initialCounter = GCounter (fromList [(1, 2), (2, 4)])
       let replica1       = increment 1 initialCounter
       let replica2       = increment 2 initialCounter
       print $ value replica1 -- 7
       print $ value replica2 -- 7
       print $ value $ merge replica1 replica2 -- 8

-- We defined laws in terms of type classes, so now we need to implement instances:
-- Hint: you already implemented these functions. :)
instance Semigroup GCounter where
   (<>) = merge

instance Monoid GCounter where
   mempty = initial

instance Semilattice GCounter where

-- Laws take care of most of our properties but it doesn't include incrementing
-- Try writing your own law for it:
incrementLaw :: GCounter -> Int -> Bool
incrementLaw c id = value (increment id c) == succ (value c)
  
type G = GCounter
  
checkAll ::  IO ()
checkAll = do
   test "associativity"  (associativity :: G -> G -> G -> Bool)
   test "left identity"  (leftIdentity  :: G -> Bool)
   test "right identity" (rightIdentity :: G -> Bool)
   test "commutativity"  (commutativity :: G -> G -> Bool)
   test "idempotency"    (idempotency   :: G -> Bool)
   test "increment"      incrementLaw
