-- SSVT Haskell Lab 
-- Week 1 - Group 5
-- Exercise 2: Test the property that a finite set A with lenght n has powerset P(A) = 2^n for integer lists of 
-- the form [1..n].
-- Deliverables: Haskell program, concise test report, answers to the questions, indication of time spent.

import Data.List (subsequences)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    forAll,
    quickCheck,
    suchThat,
    (==>),
  )

-- Our solution: Testing of the comparison of the length of an integer list to the powerset.
subsequencesLength :: Int -> Int
subsequencesLength n = length $ subsequences [1 .. n]

subsequencesLength' :: Int -> Int
subsequencesLength' n = (^ n) 2

testTheorem4 :: Int -> Property
testTheorem4 n = n >= 0 ==> subsequencesLength n == subsequencesLength' n

-- {Is the property hard to test? If you find that it is, can you given a reason why?}
-- Yes, it is hard to test due to the infinite and rapidly increasing nature of 2^n. 
-- Thus, we limited the reach of the test using a generator to 25.
genMax :: Gen Int
genMax = (arbitrary :: Gen Int) `suchThat` (<= 25)

-- Implementing QuickCheck to test our theorem.
exercise2 :: IO ()
exercise2 = do
  quickCheck $ forAll genMax testTheorem4

-- {Time spent}: 30 minutes