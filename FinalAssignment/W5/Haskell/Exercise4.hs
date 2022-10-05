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

countAll :: NrOfMutants -> [Prop] -> Fn -> Gen Int
countAll nrOfMutants props fn = testsM >>= \tests -> return $ length $ concat tests
  where
    testsM = createMatrix nrOfMutants props fn

percent :: Int -> Int -> Float
percent x y =   100 * ( a / b )
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float

strength :: Gen Int
strength = do
    survivors <- countSurvivors 300 multiplicationTableProps multiplicationTable
    all <- countAll 300 multiplicationTableProps multiplicationTable
    let killed = all - survivors
    return $ round(percent killed all)

exercise4 :: IO ()
exercise4 = do
    putStrLn "\bExercise 4\nTime spent +/- 1 hour\n"
    putStrLn "percentage: "
    percentage <- generate strength
    putStr (show percentage)
    putStrLn "%"
