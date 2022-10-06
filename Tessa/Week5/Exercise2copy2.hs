-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 2: Write a function countSurvivors that counts the number of survivors.
-- Deliverables: implementation, documentation of approach, effect of using different mutators/properties, indication of time spent.
-- Time spend: ~ 3 hours --
module FinalAssignment.W5.Haskell.Exercise2 where

import FinalAssignment.W5.Haskell.Exercise1
import FinalAssignment.W5.Haskell.MultiplicationTable
import FinalAssignment.W5.Haskell.Mutation
import Test.QuickCheck

type NrOfMutants = Int

type Prop = [Integer] -> Integer -> Bool

type Fut = (Integer -> [Integer])

type Matrix = Gen [[Maybe Bool]]

type Mutator = ([Integer] -> Gen [Integer])

allMutators :: [Mutator]
allMutators = [addElements, removeElements, addition, subtraction, multiplication, divide, modulo]

createMatrix :: Int -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> Gen [[[Bool]]]
createMatrix nrOfMutants props fut = sequence $ [vectorOf nrOfMutants (mutate' mutator props fut 3) | mutator <- allMutators]

countSurvivors :: NrOfMutants -> [Prop] -> Fut -> Gen Int
countSurvivors nrOfMutants props fut = testsMatrix >>= \tests -> return $ length $ filter (\y -> all (== True) y && not (null y)) tests
  where
    testsMatrix = fmap concat (createMatrix nrOfMutants props fut)

exercise2 :: IO ()
exercise2 = do
  y <- generate $ countSurvivors 4000 multiplicationTableProps multiplicationTable
  print y
