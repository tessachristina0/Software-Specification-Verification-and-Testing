-- SSVT Haskell Lab
-- Week 1 - Group 5
-- Exercise 4: Write a function that finds all primes < 10000 with the property that it is prime and its
-- reversal is also a prime.
-- Deliverables: Haskell program, concise test report, indication of time spent.

module Exercise4 where

import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    forAll,
    quickCheck,
    suchThat,
    (==>),
  )

-- Provided function for creating the reversal of an integer.
reversal :: Integer -> Integer
reversal = read . reverse . show

-- Provided function for checking whether a given integer n is a prime.
prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where
    xs = takeWhile (\y -> y ^ 2 <= n) primes

-- Provided function for obtaining the infinite list of primes.
primes :: [Integer]
primes = 2 : filter prime [3 ..]

-- Provided function for checking validity (truthness) of all elements of list.
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Our solution: Take the reversals of all primes smaller than 10000 then filter these
-- on the precondition of being a prime.
reversibleStream :: [Integer]
reversibleStream = filter (prime . reversal) (takeWhile (< 10000) primes)

-- * Testable properties*

-- Property 1: Output list consists of solely primes
reversibleStreamIsPrime :: [Integer] -> Bool
reversibleStreamIsPrime n = forall n prime

-- Property 2: Output list has a length of max n
reversibleStreamIsLowerThen :: Int -> [Integer] -> Bool
reversibleStreamIsLowerThen n l = length l < n

-- Property 3: Output list consists of solely reversed numbers
reversibleStreamIsReversed :: [Integer] -> Bool
reversibleStreamIsReversed n = forall n == reversal . reversal n

-- Implementing QuickCheck to test our solution.
exercise4 :: IO ()
exercise4 = do
  quickCheck $ reversibleStreamIsPrime reversibleStream
  quickCheck $ reversibleStreamIsLowerThen 10000 reversibleStream
  quickCheck $ reversibleStreamIsReversed reversibleStream

-- {Time spent}: 45 minutes (spread over 2 days)