-- SSVT Haskell Lab 
-- Week 1 - Group C
-- Exercise 1: Prove that it holds for all natural numbers n that [1^2..n^2] = (n(n+1)(2n+1))/6
-- Deliverables: Haskell program, indication of time spent.

module Exercise1 where 
import Test.QuickCheck ( (==>), quickCheck, Property )

--TODO: finish this sentence + define the properties tested below
-- Our solution: Defining the list of sumsquared natural numbers and comparing this to the sumcubed* ...
sumSquared :: Integer -> Integer
sumSquared n = sum [(^2) k | k <- [1..n]]

sumSquared' :: Integer -> Integer
sumSquared' n = (n * (n + 1) * (2 * n + 1)) `div` 6

sumCubed :: Integer -> Integer
sumCubed n = sum [(^3) k | k <- [1 .. n]]

sumCubed' :: Integer -> Integer
sumCubed' n = (^ 2) ((n * (n + 1)) `div` 2)

-- Property 1: 
testTheorem2 :: Integer -> Property
testTheorem2 n = n >= 0 ==> sumSquared n == sumSquared' n

-- Property 2:
testTheorem3 :: Integer -> Property
testTheorem3 n = n >= 0 ==> sumCubed n == sumCubed' n

-- Implementing QuickCheck to test our theorems.
exercise1 :: IO()
exercise1 = do
    quickCheck testTheorem2
    quickCheck testTheorem3

-- {Time spent}: 40 minutes (including figuring out how to use QuickCheck method)