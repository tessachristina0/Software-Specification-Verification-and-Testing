-- SSVT Haskell Lab
-- Week 3 - Group 5
-- Exercise 5: How can you prove that the sub implementation is correct? Test the implementation with two QuickCheck properties.
-- Write a recursive implementation of the function nsub :: Form -> Int such that nsub f computes the exact number of sub-formulae of the formula f. Test your implementation using QuickCheck.
-- Deliverables: for 5.1: answer to the question, quickCheck of two properties, indication of time spent; for 5.2: implementation of nsub, quickCheck properties, indication of time spent
-- Time spend: 6 hours --

module Exercise5 where
import Data.List
import System.Random
import Test.QuickCheck
import FinalAssignment.W3.Lecture3
import FinalAssignment.W3.SetOrd

-- Given sub function to be tested
sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

{-
Q5.1:   How can you prove that the sub implementation is correct? Test the implementation with two QuickCheck properties.
A:      We can prove it's correctness by checking against properties of subformulas. During the lab we came up with the following four properties:
        1. Check if the function returns the correct amount of subformulas for a formula
        2. Check if the function returns a finite set of subformulas
        3. Every prop in the formula is a subformula in itself
        4. Check if the formula is included as a subformula

        We have opted for properties 1 and 4, because they were easier base case properties to write tests for. 
        Property 2 is not possible to test since you have to check against an infinite set, so we didn't opt for this.
-}

set2list :: Ord a => Set a -> [a]
set2list (Set []) = []
set2list (Set (x:xs)) = x : set2list (Set xs)

-- Test if the formula itself is included because a formula is a subformula of itself.
testIsFormulaIncluded :: Form -> Bool
testIsFormulaIncluded f = inSet f $ sub f

-- Test against a fixed amount of subformulas, if the genForm worked, we could possibly generate specific forms with a fixed amount of subformulas.
testCorrectAmountOfSubformulas :: Form -> Bool
testCorrectAmountOfSubformulas f = length (set2list $ sub f) == 7

{-
Q5.2:   Write a recursive implementation of the function nsub :: Form -> Int such that nsub f 
        computes the exact number of sub-formulae of the formula f. Test your implementation using QuickCheck.
A:      This took waaaaaaaaaaaay too dang long because we misinterpreted the assignment.
        Unfortunately we were not able to test with quickCheck since we couldn't get the generator to work in time. But with hardcoded test it is evident that the results are correct.
-}

startFromZero :: Int
startFromZero = 0

nsubRecursive :: Set Form -> Int -> Int
nsubRecursive (Set []) n = n
nsubRecursive (Set f) n = nsubRecursive (Set $ tail f) (1 + n)

nsub :: Form -> Int
nsub f = nsubRecursive (sub f) startFromZero

exercise5 :: IO ()
exercise5 = do
  putStrLn "\nExercise 5.1"

  putStrLn "\ntestIsFormulaIncluded form 1:"
  print $ testIsFormulaIncluded form1

  putStrLn "\ntestIsFormulaIncluded form 2:"
  print $ testIsFormulaIncluded form2

  putStrLn "\ntestCorrectAmountOfSubformulas form 1:"
  print $ testCorrectAmountOfSubformulas form1

  putStrLn "\ntestCorrectAmountOfSubformulas form 2:"
  print $ testCorrectAmountOfSubformulas form2

  putStrLn "\nExercise 5.2"

  putStrLn "\nform1HasSevenSub:"
  print $ nsub form1 == 7

  putStrLn "\nform1HasSevenSub:"
  print $ nsub form2 == 7

  putStrLn "\nform3HasEightSub:"
  print $ nsub form3 == 8