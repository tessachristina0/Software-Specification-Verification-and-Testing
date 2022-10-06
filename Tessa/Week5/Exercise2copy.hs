-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 2: Write a function countSurvivors that counts the number of survivors.
-- Deliverables: implementation, documentation of approach, effect of using different mutators/properties, indication of time spent.
-- Time spend: ~ 3 hours --
module FinalAssignment.W5.Haskell.Exercise2 where

import FinalAssignment.W5.Haskell.MultiplicationTable
import FinalAssignment.W5.Haskell.Mutation
import FinalAssignment.W5.Haskell.Exercise1
import Test.QuickCheck


type NrOfMutants = Int
type Prop = [Integer] -> Integer -> Bool
type Fut = (Integer -> [Integer])
type Matrix = Gen [[Maybe Bool]]

allMutators :: [[Integer] -> Gen [Integer]]
allMutators = [addElements, removeElements, addition, subtraction, multiplication, divide, modulo]


createMatrix :: NrOfMutants -> [Prop] -> Fut -> Matrix
createMatrix nrOfMutants props fut = sequence $ [vectorOf nrOfMutants (mutate mutator prop fut 3) | prop <- props, mutator <- allMutators]


-- Applies a mutator to a property and function under test, then returns whether the mutant is killed (False), whether it lives (True), or that the mutant did not change the output (empty list)
mutate' :: Eq a => (a -> Gen a) -> [Prop] -> (Integer -> a) -> Integer -> Gen [Bool]
mutate' mutator prop fut input = mutation >>= \mutant -> mutateOrNothing' output mutant (propertyExecutor' prop mutation input)
        where output = fut input
              mutation = mutator output
-- So, this only creates the mutants (outputs) by applying mutator to the fut. Then applies execution to create list of 1 mutant.

-- Applies all properties by comparing the regular input and the mutated (mut) output (of each n muts) to check if Prop holds per 1 mutation.
propertyExecutor' :: Eq a => [a -> Integer -> Bool] -> Gen a -> Integer -> Gen [Bool]
propertyExecutor' prop o x = o >>= \output -> return $ map (\y -> y output x) prop
-- Thus, output is a list of False/True per prop of 1 mutation.

-- Do mutate' but then on n times, so get a list of n lists (takes each list om en om, until n is reached) 
-- So n should be meervoud van 5? 
createMatrix' :: NrOfMutants -> [Prop] -> Fut -> Matrix
createMatrix' n props fut = sequence $ [vectorOf n (mutate' mutator props fut 3) | mutator <- allMutators]

-- Then, remove empty lists (failed mutation)
-- Then, check each mutation-list if all is True --> a survivor. Count how many lists those are.
countSurvivors :: NrOfMutants -> [Prop] -> Fut -> Gen Int
countSurvivors nrOfMutants props fut = testsMatrix >>= \tests -> return $ length $ filter (== Just True) $ concat tests
  where
    testsMatrix = createMatrix nrOfMutants props fut

exercise2 :: IO ()
exercise2 = do
  y <- generate $ countSurvivors 4000 multiplicationTableProps multiplicationTable
  print y
