-- SSVT Haskell Lab
-- Week 1 - Group 5
-- Exercise 1: Prove that it holds for all natural numbers n that [1^2..n^2] = (n(n+1)(2n+1))/6
-- Deliverables: Haskell program, indication of time spent.

module Exercise1 where

import Test.QuickCheck (Property, quickCheck, (==>))

-- Our solution: Defining the list of sumsquared natural numbers and comparing both squared as cubed functions.
sumSquared :: Integer -> Integer
sumSquared n = sum [(^ 2) k | k <- [1 .. n]]

sumSquared' :: Integer -> Integer
sumSquared' n = (n * (n + 1) * (2 * n + 1)) `div` 6

sumCubed :: Integer -> Integer
sumCubed n = sum [(^ 3) k | k <- [1 .. n]]

sumCubed' :: Integer -> Integer
sumCubed' n = (^ 2) ((n * (n + 1)) `div` 2)

-- Property 1: for natural numbers (so all numbers bigger then 0) the squared functions are compared.
testTheorem2 :: Integer -> Property
testTheorem2 n = n >= 0 ==> sumSquared n == sumSquared' n

-- Property 2: for all natural numbers the cubed functions are compared.
testTheorem3 :: Integer -> Property
testTheorem3 n = n >= 0 ==> sumCubed n == sumCubed' n

-- Implementing QuickCheck to test our theorems.
exercise1 :: IO ()
exercise1 = do
  quickCheck testTheorem2
  quickCheck testTheorem3

-- {Time spent}: 40 minutes (including figuring out how to use QuickCheck method)