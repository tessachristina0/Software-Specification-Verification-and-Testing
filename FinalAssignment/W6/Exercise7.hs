-- SSVT Haskell Lab
-- Week 6 - Group 5
-- Exercise 7: Is there a difference between the symmetric closure of the transitive closure of a relation R and the transitive closure of the symmetric closure of R 
-- Deliverables: Haskell file with the answer in comment form, indication of time spent
-- Time spend: 1.5 hours --
module FinalAssignment.W6.Exercise7 where
import FinalAssignment.W6.SetOrd
import Data.List
import Test.QuickCheck
import FinalAssignment.W6.Exercise5 ( trClos )
import FinalAssignment.W6.Exercise3 ( Rel, symClos )

-- Function to compare the symClos of the trClos on relations with the trClos of the symClos of relations
-- This has to show us a counterexample in the output for some properties.
prop_test_relations :: Ord a => Rel a -> Bool
prop_test_relations relations = symClos (trClos relations) == trClos (symClos relations)

exercise7 :: IO ()
exercise7 = do
    putStrLn "\bExercise 1\nTime spent +/- 1.5 hours\n"
    putStrLn "We think there is a difference between the two and therefore we have given you two examples. An example which passes and a counterexample which fails.\n"
    putStrLn "example that it could be the same: "
    print $ prop_test_relations [(1,1)]
    putStrLn "example that it is not the same: "
    print $ prop_test_relations [(1,2),(2,3),(3,4)]
