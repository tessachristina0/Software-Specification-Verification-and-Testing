-- SSVT Haskell Lab
-- Week 1 - Group 5
-- Exercise 5: Find the smallest prime number that is a sum of 101 consecutive primes.
-- Deliverables: Haskell program, solution, answer to the questions, indication of time spent.

module Exercise5 where

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

-- Function for selecting the first 101 elements of a list.
take101 :: [Integer] -> [Integer]
take101 xs = takeWhile (<= 101) xs

-- Function for checking if the selected primes sum up to a prime.
sumPrimes :: [Integer] -> Integer
sumPrimes xs
  | prime y = y
  | otherwise = sumPrimes (take101 (tail xs))
  where
    y = sum xs

-- Our solution: Recursively checking whether 101 consecutive primes sum to a prime,
-- else moving the selection one element up.
consecutive101Prime :: Integer
consecutive101Prime = sumPrimes (take101 primes)

-- {Do you have to test that your answer is correct?}
-- Yes, you should always check whether your function has valid output.

-- {How could this be checked?}
-- That seems difficult to do, other than finding an alternative algorithm on for instance Google that could
-- validate its method mathematically. Thusfar, I have not been able to find a way to do so myself.

-- {Time spent}: 30 minutes