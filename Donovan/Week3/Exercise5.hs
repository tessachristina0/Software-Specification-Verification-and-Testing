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
-}

totalCount :: (Form, Int) -> (Form, Int) -> (Form, Int)
totalCount f1 f2    = (fst f1, snd f1 + snd f2)

nsub :: Form -> Int
nsub f = snd (nsub' f 0)

nsub' :: Form -> Int -> (Form, Int)
nsub' (Prop x) n = (Prop x, 1 + n)
nsub' (Neg f) n = nsub' f (1 + n)
nsub' f@(Cnj [f1,f2]) n = totalCount (nsub' f1 (1 + n)) (nsub' f2 n)
nsub' f@(Dsj [f1,f2]) n = totalCount (nsub' f1 (1 + n)) (nsub' f2 n)
nsub' f@(Impl f1 f2) n = totalCount (nsub' f1 (1 + n)) (nsub' f2 n)
nsub' f@(Equiv f1 f2) n = totalCount (nsub' f1 (1 + n)) (nsub' f2 n)

-- main = do
    -- testIsFormulaIncluded form1
    -- putStrLn "3 hours"
    -- testCorrectAmountOfSubformulas form1