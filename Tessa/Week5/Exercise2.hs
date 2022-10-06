-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 2: Write a function countSurvivors that counts the number of survivors.
-- Deliverables: implementation, documentation of approach, effect of using different mutators/properties, indication of time spent.
-- Time spend: ~ hours --
module FinalAssignment.W5.Haskell.Exercise2 where
import FinalAssignment.W5.Haskell.Mutation
import FinalAssignment.W5.Haskell.MultiplicationTable


{- Note:
In this lab, we write our own mutation testing framework like FitSpec. We manipulate the output data of a 
function under test (FUT), to check that any incorrect output for a given input fails the tests. This way, 
we can discover survivors, the minimal set of properties, and find all the conjectures.
-}



{-
Write a function countSurvivors that counts the number of survivors...
Where:
  first argument    = number of mutants (4000 in the FitSpec example).
  second argument   = list of properties.
  third argument    = function under test (the multiplication table function in this case).
  output            = number of surviving mutants (0 in the FitSpec example).
-}

countSurvivors :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> Integer



-- Document the effect of which mutations are used + which properties are used, on the number of survivors.
exercise2 :: IO ()
exercise2 = do
  putStrLn "\bExercise 1\nTime spent +/- ~ hours\n"
  putStrLn "\n"
