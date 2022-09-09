-- SSVT Haskell Lab
-- Week 1 - Group 5
-- Exercise 6: Write a Haskell function using counterexamples that can be used to refute the following conjecture:
-- "If [p1..pn] is a list of consecutive primes starting from 2, then (p1*..*pn)+1 is also prime."
-- Deliverables: Haskell program, indication of time spent.

module Exercise6 where

import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    forAll,
    quickCheck,
    suchThat,
    (==>),
  )

-- Provided function for checking whether a given integer n is a prime.
prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where
    xs = takeWhile (\y -> y ^ 2 <= n) primes

-- Provided function for obtaining the infinite list of primes.
primes :: [Integer]
primes = 2 : filter prime [3 ..]

-- Function to obtain a list of n consecutive primes.
getPrimesList :: [[Integer]]
getPrimesList = [take n primes | n <- [2 ..]]

-- Function for obtaining a list of tuples consisting of a prime and the product of the primelist + 1.
counterexamples :: [([Integer], Integer)]
counterexamples = [(primeList, product primeList + 1) | primeList <- getPrimesList, not (prime (product primeList + 1))]

-- Function to show the first generated counterexample.
smallestItemCounterExamples :: ([Integer], Integer)
smallestItemCounterExamples = head counterexamples

-- Implementing QuickCheck to test our function validity.
exercise6 :: IO ()
exercise6 = do
  print smallestItemCounterExamples

-- {Time spent}: 35 minutes