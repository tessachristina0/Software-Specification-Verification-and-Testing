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

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversibleStream :: [Integer]
reversibleStream = filter (prime . reversal) (takeWhile (<10000) primes)

-- Testable properties
reversibleStreamIsPrime :: [Integer] -> Bool
reversibleStreamIsPrime n = forall n prime

reversibleStreamIsLowerThen :: Int -> [Integer] -> Bool
reversibleStreamIsLowerThen n l = length l < n

reversibleStreamIsReversed :: [Integer] -> Bool
reversibleStreamIsReversed _  = False -- TODO: Implement function

-- TODO: Remove Test smaller than 10.000 & list is reversed
exercise4 :: IO()
exercise4 = do
  quickCheck $ reversibleStreamIsPrime reversibleStream
  quickCheck $ reversibleStreamIsLowerThen 10000 reversibleStream
  quickCheck $ reversibleStreamIsReversed reversibleStream