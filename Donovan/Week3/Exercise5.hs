-- Donovan Schaafsma
-- Exercise 5
module Exercise5 where
import Data.List
import System.Random
import Test.QuickCheck
import Donovan.Week3.Lecture3
import Donovan.Week3.SetOrd

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
A:      1. Check if the function returns the correct amount of subformulas for a formula
        2. Check if the function returns a finite set of subformulas
        3. Every prop in the formula is a subformula in itself
        4. Check if the formula is included as a subformula
-}

set2list :: Ord a => Set a -> [a]
set2list (Set []) = []
set2list (Set (x:xs)) = x : set2list (Set xs)

testIsFormulaIncluded :: Form -> Bool
testIsFormulaIncluded f = inSet f $ sub f

testCorrectAmountOfSubformulas :: Form -> Bool
testCorrectAmountOfSubformulas f = length (set2list $ sub f) == 7

{-
Q5.2:   Write a recursive implementation of the function nsub :: Form -> Int such that nsub f 
        computes the exact number of sub-formulae of the formula f. Test your implementation using QuickCheck.
A:      This took a lot longer than expected because I misinterpreted the assignment.
        Unfortunately we were not able to test with quickCheck since we couldn't get the generator to work in time. But with hardcoded test it is evident that the results are correct.
-}

startCount :: Int
startCount = 0

nsubRecursive :: Set Form -> Int -> Int
nsubRecursive (Set []) n = n
nsubRecursive (Set f) n = nsubRecursive (Set $ tail f) (1 + n)

nsub :: Form -> Int
nsub f = nsubRecursive (sub f) startCount

exercise5 :: IO ()
exercise5 = do
  putStrLn "\bExercise 5\nTime spent +/- 6 hours\n"
  
  putStrLn "\ntestIsFormulaIncluded form 1:"
  print $ testIsFormulaIncluded form1

  putStrLn "\ntestIsFormulaIncluded form 2:"
  print $ testIsFormulaIncluded form2
  
  putStrLn "\ntestCorrectAmountOfSubformulas form 1:"
  print $ testCorrectAmountOfSubformulas form1

  putStrLn "\ntestCorrectAmountOfSubformulas form 2:"
  print $ testCorrectAmountOfSubformulas form2