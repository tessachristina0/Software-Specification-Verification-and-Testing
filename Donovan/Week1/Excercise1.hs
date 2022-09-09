import Test.QuickCheck

theorem :: Integer -> Bool
theorem n = sum [1..n] == (n * (n + 1)) `div` 2

-- Excercise 1

question2 :: Integer -> Integer
question2 n = (n (n + 1) * ((2 * n) + 1)) `div` 6

question3 :: Integer -> Integer
-- question3 n = 