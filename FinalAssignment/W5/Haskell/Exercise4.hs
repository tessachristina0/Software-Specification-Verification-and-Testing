-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 4: Implement a function that calculates the strength of a given set of properties, which is the percentage of mutants they kill.
-- Deliverables: implementation, documentation of approach, indication of time spent.
-- Time spend: 1 hour --
module FinalAssignment.W5.Haskell.Exercise4 where
import FinalAssignment.W5.Haskell.Mutation
import FinalAssignment.W5.Haskell.MultiplicationTable
import FinalAssignment.W5.Haskell.Exercise2
import Test.QuickCheck

-- Function to count all of the mutants to compare with the survivors
countAll :: NrOfMutants -> [Prop] -> Fn -> Gen Int
countAll nrOfMutants props fn = testsM >>= \tests -> return $ length $ concat tests
  where
    testsM = createMatrix nrOfMutants props fn

-- Function to calculate the percentage and return this in a float
calcPercentage :: Int -> Int -> Float
calcPercentage x y = 100 * ( (fromIntegral x :: Float) / (fromIntegral y :: Float) )

-- Function to calculate the strength of a given set properties, which is the percentage of killed mutants
strength :: Gen Int
strength = do
    survivors <- countSurvivors 10000 multiplicationTableProps multiplicationTable
    all <- countAll 10000 multiplicationTableProps multiplicationTable
    let killed = all - survivors
    return $ round $ calcPercentage killed all

{-
  This exercise is done by calculating the percentage of killed mutants. Which is done by retrieving the survivors and calculate the
  full amount of mutants. This is done by two helper functions to count all the mutants and to calculate the percentage.
-}

exercise4 :: IO ()
exercise4 = do
    putStrLn "\bExercise 4\nTime spent +/- 1 hour\n"
    putStrLn "Percentage: "
    percentage <- generate strength
    putStr (show percentage)
    putStrLn "%"
