-- SSVT Haskell Lab
-- Week 3 - Group 5
-- Exercise 6: Transform the CNF to a CLS and write tests for it using automated testing.
-- Deliverables: Conversion program, test generator, test properties, documentation of the automated testing process. Also, give an indication of time spent.
module FinalAssignment.W3.Exercise6 where

import Data.List (nub, sort, (\\))
import FinalAssignment.W3.Exercise3 (cnf)
import FinalAssignment.W3.Lecture3
import System.Random ()
import Test.QuickCheck (quickCheck)
import FinalAssignment.W3.Exercise4
import Test.QuickCheck.Property (forAll)
import Debug.Trace
import Test.QuickCheck.Test


type Clause = [Int]
type Clauses = [Clause]

-- Function to retrieve the property (Prop f) and check wether this is a negation or not.
-- Then return the value of an integer. For a negation this is shown with a minus. For an absolute integer this return just the integer.
propToInt :: Form -> Int
propToInt (Prop f) = f
propToInt (Neg (Prop f)) = - f

-- Function to parse properties from disjunctions.
formToClause :: Form -> Clause
formToClause (Dsj fs) = map propToInt fs
formToClause f = formToClause (Dsj [f])

-- Function to parse disjunctions a conjunctions
cnf2cls :: Form -> Clauses
cnf2cls (Cnj xs) = map formToClause xs
cnf2cls f = cnf2cls (Cnj [f])

-- Function to show the actual cls using the cnf2cls function
-- The cnf2cls function requires a cnf so the cnf fucntion is called with the form which has to show the cls.
cls :: Form -> Clauses
cls f = cnf2cls $ cnf f

-- Function to retrieve the integers of absolute numbers and remove the duplicates
-- So this function returns a list of integers which are present in the clauses
clsPropNames :: Clauses -> [Name]
clsPropNames c = sort $ nub (map abs (concat c))

-- Function to compare the cls properties to the properties of the cnf of the same form
prop_sameProperties :: Form -> Bool
prop_sameProperties f = sort (clsPropNames (cls f)) == sort (propNames (cnf f))

-- Function to count the clauses of a form
-- If there is no list of conjunctions then zero is returned
countClauses :: Form -> Int
countClauses (Cnj fs) = length fs
countClauses f = 0

-- Function to test the amount of clauses in a form
testAmountOfClauses :: Form -> Bool
testAmountOfClauses f = countClauses (cnf f) == length (cls f)

{-
  So we are testing two cases of a clause, the amount of clauses in a form. And equality of the properties. This checks if the integers in the cls are equal to the integers
  in the cnf. We have tested these two things to make sure there are no other integers created during the transformation to a cls. Then the times a clause is inside a form
  makes sure the clauses of a cnf and a cls are equal. So this ensures us the clauses are both equal in size as using the same integers.

  These tests do not validate the correctness of the function which converts a cnf to a cls. We have tried to implement this but ran out of time. These tests are
  necessary to complete the tests of this implementation. We think this can be done by creating a transformer back to cnf and the compare the equivalence.
-}

exercise6 :: IO ()
exercise6 = do
  putStrLn "\bExercise 6\nTime spent +/- 6 minutes\n"
  verboseCheck $ forAll (genForm 5) prop_sameProperties
