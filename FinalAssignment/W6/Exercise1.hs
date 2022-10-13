-- SSVT Haskell Lab
-- Week 6 - Group 5
-- Exercise 1: Implement a random data generator for the datatype Set Int, where Set is as defined in SetOrd.hs. First do this from scratch, next give a version that uses QuickCheck to random test this datatype.
-- Deliverables: two random test generators, indication of time spent.
-- Time spend: 1 hour --
module FinalAssignment.W6.Exercise1 where
import FinalAssignment.W6.SetOrd
import Data.List
import System.Random
import Test.QuickCheck ( choose, generate )
import Control.Monad

-- Function to create a random list
randomList :: Int -> Int -> Int -> IO [Int]
randomList num x y = replicateM num (randomRIO (x,y::Int))

-- Function to translate a random list into a set
randomSet :: Int -> Int -> Int -> IO (Set Int)
randomSet num x y = do
    list <- randomList num x y
    return (list2set list)

-- Function to create a random list using the quickcheck generate with choose, which generates a random element in a given range
randomListQuickCheck :: Int -> Int -> Int -> IO [Int]
randomListQuickCheck num x y = replicateM num (generate $ choose(x, y))

-- Function to translate a random list into a set but then for the quickcheck function
randomSetQuickCheck :: Int -> Int -> Int -> IO (Set Int)
randomSetQuickCheck num x y = do
    list <- randomListQuickCheck num x y
    return (list2set list)

exercise1 :: IO ()
exercise1 = do
    putStrLn "\bExercise 1\nTime spent +/- 1 hour\n"
    putStrLn "Random Set: "
    randomSet <- randomSet 20 0 100
    print randomSet
    putStrLn "Random Set using QuickCheck: "
    randomSetQuickCheck <- randomSetQuickCheck 20 0 100
    print randomSetQuickCheck