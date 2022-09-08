-- René Kok
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
testTheorem2 :: Integer -> Bool
testTheorem2 n = sumSquared n == sumSquared' n

testTheorem3 :: Integer -> Bool
testTheorem3 n =  sumCubed n == sumCubed' n