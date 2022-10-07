-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 2: Write a function countSurvivors that counts the number of survivors.
-- Deliverables: implementation, documentation of approach, effect of using different mutators/properties, indication of time spent.
-- Time spend: ~ 3 hours --
module Exercise2 where

import Exercise1
import MultiplicationTable
import Mutation
import Test.QuickCheck

type NrOfMutants = Int

type Prop = [Integer] -> Integer -> Bool

type Fn = (Integer -> [Integer])

type Matrix = Gen [[Maybe Bool]]

type Mutator = ([Integer] -> Gen [Integer])

allMutators :: [Mutator]
allMutators = [addElements, removeElements, addition, subtraction, multiplication, divide, modulo]

createMatrix :: Int -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> Gen [[[Bool]]]
createMatrix nrOfMutants props fn = sequence $ [vectorOf nrOfMutants (mutate' mutator props fn 3) | mutator <- allMutators]

countSurvivors :: NrOfMutants -> [Prop] -> Fn -> Gen Int
countSurvivors nrOfMutants props fn = testsMatrix >>= \tests -> return $ length $ filter (\y -> all (== True) y && not (null y)) tests
  where
    testsMatrix = fmap concat (createMatrix nrOfMutants props fn)

exercise2 :: IO ()
exercise2 = do
  y <- generate $ countSurvivors 4000 multiplicationTableProps multiplicationTable
  print y
