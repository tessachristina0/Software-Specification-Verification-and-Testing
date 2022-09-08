-- René Kok
module Exercise1 where 
import Test.QuickCheck

-- Exercise 1
sumSquared :: Integer -> Integer
sumSquared n = sum [(^2) k | k <- [1..n]]

sumSquared' :: Integer -> Integer
sumSquared' n = (n * (n + 1) * (2 * n + 1)) `div` 6

sumCubed :: Integer -> Integer
sumCubed n = sum [(^3) k | k <- [1 .. n]]

sumCubed' :: Integer -> Integer
sumCubed' n = (^ 2) ((n * (n + 1)) `div` 2)

-- Quicktest
testTheorem2 :: Integer -> Property
testTheorem2 n = n >= 0 ==> sumSquared n == sumSquared' n

testTheorem3 :: Integer -> Property
testTheorem3 n = n >= 0 ==> sumCubed n == sumCubed' n

exercise1 :: IO()
exercise1 = do
    quickCheck testTheorem3
