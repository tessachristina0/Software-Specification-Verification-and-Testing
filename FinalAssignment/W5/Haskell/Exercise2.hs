-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 2: Write a function countSurvivors that counts the number of survivors.
-- Deliverables: implementation, documentation of approach, effect of using different mutators/properties, indication of time spent.
-- Time spend: ~ 3 hours --
module FinalAssignment.W5.Haskell.Exercise2 where

import FinalAssignment.W5.Haskell.MultiplicationTable
import FinalAssignment.W5.Haskell.Mutation
import Test.QuickCheck
import FinalAssignment.W5.Haskell.Exercise1

type NrOfMutants = Int
type Prop = [Integer] -> Integer -> Bool
type Fn = (Integer -> [Integer])
type Matrix = Gen [[Maybe Bool]]

createMatrix :: NrOfMutants -> [Prop] -> Fn -> Matrix
createMatrix nrOfMutants props fn = sequence $ [vectorOf nrOfMutants (mutate mutator prop fn 3) | prop <- props, mutator <- allMutators]

allMutators :: [[Integer] -> Gen [Integer]]
allMutators = [addElements, removeElements, addition, subtraction, multiplication, divide, modulo]

countSurvivors :: NrOfMutants -> [Prop] -> Fn -> Gen Int
countSurvivors nrOfMutants props fn = testsM >>= \tests -> return $ length $ filter (== Just True) $ concat tests
  where
    testsM = createMatrix nrOfMutants props fn

exercise2 :: IO ()
exercise2 = do
  y <- generate $ countSurvivors 10000 multiplicationTableProps multiplicationTable
  print y
