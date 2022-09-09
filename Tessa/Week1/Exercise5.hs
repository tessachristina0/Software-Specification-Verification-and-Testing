
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

-- The number 101 is a prime, and it is also the sum of five consecutive primes, 
-- namely 13+17+19+23+29. Find the smallest prime number that is a sum of 101 consecutive primes.

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- primes' :: [Integer]
-- primes' = take 10000 primes

takePrimes :: [Integer] -> [Integer]
takePrimes xs = takeWhile (<= 101) xs 

countPrimes :: [Integer] -> Integer
countPrimes xs | prime y    = y
               | otherwise    = countPrimes (takePrimes (tail xs))
               where 
                    y = sum xs

-- Follow this type declaration:
consecutive101Prime :: Integer
consecutive101Prime = countPrimes (takePrimes primes)


-- take first 101 prime nrs from [primes] and call this [myprimes]
-- sum [myprimes] = y
-- check if sum is primenr with "isPrime" 
-- if not: remove indexnr1 from [myprimes]
-- add prime i = indexnr101+1 from [primes] to end [myprimes]
-- sum new [myprimes] = y
-- check if sum is primenr with "isPrime"
-- if not: remove indexnr1 from [myprimes]
-- add new prime i+1 from [primes] to end [myprimes]

