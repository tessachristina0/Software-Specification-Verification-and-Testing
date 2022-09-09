-- SSVT Haskell Lab 
-- Week 1 - Group C
-- Exercise 3: Test the property that a permutation of a list is a reordering of the members of a list 
-- for integer lists of the form [1..n], and find a formula for permutations of a list of n distinct objects.
-- Deliverables: Haskell program, concise test report, answers to the questions, indication of time spent.

import Data.List (permutations)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    forAll,
    quickCheck,
    suchThat,
    (==>),
  )

-- Provided function for creating the permutations of a list.
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (insrt x) (perms xs))
  where
    insrt x [] = [[x]]
    insrt x (y : ys) = (x : y : ys) : map (y :) (insrt x ys)

-- TODO: explanation of solution
-- Our solution: ...
testTheorem5 :: Int -> Property
testTheorem5 n = n >= 0 ==> length (perms [1 .. n]) == length (permutations [1 .. n])


-- {Is the property hard to test? If you find that it is, can you given a reason why?}
-- Yes, it is hard to test due to ... thus, we have used a generator to create a maximum for ...

genMax :: Gen Int
genMax = (arbitrary :: Gen Int) `suchThat` (<= 9)

-- TODO: Answer the following questions...
-- {When you perform the test for exercise 5, what are you testing actually?}
-- 
-- {Are you checking a mathematical fact?}
-- 
-- {Or are you testing whether perms satisfies a part of its specification? Or are you testing something else still?}
-- 

exercise3 :: IO ()
exercise3 = do
  quickCheck $ forAll genMax testTheorem5

-- {Time spent}: 30 minutes