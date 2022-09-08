import Data.List (subsequences)
import Test.QuickCheck (quickCheck, Property, (==>))

subsequencesLength :: Int -> Int
subsequencesLength n = length $ subsequences [1..n]

subsequencesLength' :: Int -> Int
subsequencesLength' n = (^n) 2

testTheorem4 :: Int -> Property
testTheorem4 n = n >= 0 ==> subsequencesLength n == subsequencesLength' n

exercise2 :: IO()
exercise2 = do
    quickCheck testTheorem4