import Data.List
import Data.Numbers.Primes

module Exercise3 where 
import Test.QuickCheck


reversal :: Integer -> Integer
reversal = read . reverse . show

-- 3 properties
---- could be preconditions to functions
-- quickcheck is precondition
-- Quickcheck generates input for preconditio
-- slide 31: preconditions

-- precondition is a property of the input of the function tested


-- this should be the solution (function) that will be tested with Quickcheck
-- stream of less than 10000 (lazy list)
reversibleStream :: [Integer]
reversibleStream = 

propPrime :: Property
propPrime = isPrime 


main :: IO()
main = do 
    quickCheck propPrime