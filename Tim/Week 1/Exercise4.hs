-- Tim van Ekert
-- Exercise 4
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

reversal :: Integer -> Integer
reversal = read . reverse . show

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- Quicktest

reversibleStream :: [Integer]
reversibleStream = filter (prime . reversal) (takeWhile (<10000) primes)

-- exercise4 :: IO()
-- exercise4 = do
--   quickCheck reversibleStream