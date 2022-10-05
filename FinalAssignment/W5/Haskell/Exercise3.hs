-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 3: Implement a function that calculates the minimal property subsets, given a 'function under test' and a set of properties
-- Deliverables: implementation, documentation of approach, indication of time spent.
-- Time spend: ~ hours --
module FinalAssignment.W5.Haskell.Exercise3 where
import FinalAssignment.W5.Haskell.Mutation
import FinalAssignment.W5.Haskell.MultiplicationTable
import FinalAssignment.W5.Haskell.Exercise2
import Test.QuickCheck

-- Conjuctie, Disjunctie en equiv

-- Stap 1: Per property ga je elke mutator af
-- Stap 2: Alle resultaten checken


-- Gen [[Maybe Bool]]


-- createMatrix' _ [] _ _ = []
createMatrix' nrOfMutants props mutators fn = aa >>= \a -> return (concat a) -- ++ createMatrix' nrOfMutants (tail props) mutators fn
  where aa = sequence $ [vectorOf nrOfMutants (mutate mut (head props) fn 3) | mut <- mutators]


ee nrOfMutants props mutators fn = concatMap (\prop -> concatMap (\mut -> [vectorOf nrOfMutants (mutate mut prop fn 3) ]) mutators) props

ff nrOfMutants props mutators fn = concatMap x props
  where 
    x = \prop -> [vectorOf nrOfMutants (mutate mut prop fn 3) | mut <- mutators]
--    y = if prop == 
  

dd = generate $ sequence $ ee 1 [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput] [addElements, removeElements] multiplicationTable

exercise3 :: IO ()
exercise3 = do
  putStrLn "\bExercise 1\nTime spent +/- ~ hours\n"
  putStrLn "\n"
