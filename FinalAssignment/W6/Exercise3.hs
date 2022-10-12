-- SSVT Haskell Lab
-- Week 6 - Group 5
-- Exercise 3: Suppose we implement binary relations as list of pairs, Haskell type [(a,a)]. Assume the following definition:
-- to define a function that gives the symmetric closure of a relation, where the relation is represented as an ordered list of pairs. E.g., symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].
-- Deliverables: Haskell program, indication of time spent.
-- Time spend: 1 hour --
module FinalAssignment.W6.Exercise3 where
import FinalAssignment.W6.SetOrd
import Data.List
import System.Random
import Test.QuickCheck
import Control.Monad

type Rel a = [(a,a)]

-- Function to give the symmetric closure of a relation. The tuple is inversed and printed as well in the array, this is done for each value.
symClos :: Ord a => Rel a -> Rel a
symClos relations = sort (relations `union` [ (x,y) | (y,x) <- relations])

exercise3 :: IO ()
exercise3 = do
    putStrLn "\bExercise 1\nTime spent +/- 1 hour\n"
    putStrLn "symClos example: "
    print $ symClos [(1,2),(2,3),(3,4)]
    putStrLn "symClos extra example: "
    print $ symClos [(1,2),(2,3),(3,4),(4,5),(5,6),(6,7)]