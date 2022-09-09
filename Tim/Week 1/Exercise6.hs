-- Tim van Ekert
-- Exercise 6
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

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

getPrimesList :: [[Integer]]
getPrimesList = [ take n primes | n <- [2..] ]

counterexamples :: [([Integer],Integer)]
counterexamples = [ (primeList,product primeList + 1) | primeList <- getPrimesList, not (prime (product primeList + 1)) ]

smallestItemCounterExamples :: ([Integer], Integer)
smallestItemCounterExamples = head counterexamples

exercise6 :: IO()
exercise6 = do
    print smallestItemCounterExamples
