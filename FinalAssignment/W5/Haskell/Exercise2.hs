-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 2: Write a function countSurvivors that counts the number of survivors.
-- Deliverables: implementation, documentation of approach, effect of using different mutators/properties, indication of time spent.
-- Time spend: ~ hours --
module FinalAssignment.W5.Haskell.Exercise2 where

import FinalAssignment.W5.Haskell.MultiplicationTable
import FinalAssignment.W5.Haskell.Mutation
import Test.QuickCheck
import Data.Maybe

allMutators :: [[Integer] -> Gen [Integer]]
allMutators = [addElements, removeElements]

countSurvivors :: Int -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> Gen Int
countSurvivors nrOfMutants props fn = testsM >>= \tests -> return $ length $ filter (== Just False) $ concat tests
  where testsM = sequence [ vectorOf nrOfMutants (mutate mutator prop fn 3) | prop <- props, mutator <- allMutators]

exercise2 :: IO ()
exercise2 = do
  y <- generate $ countSurvivors 300 multiplicationTableProps multiplicationTable
  print y
