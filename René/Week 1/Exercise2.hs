import Data.List (subsequences)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    forAll,
    quickCheck,
    suchThat,
    (==>),
  )

subsequencesLength :: Int -> Int
subsequencesLength n = length $ subsequences [1 .. n]

subsequencesLength' :: Int -> Int
subsequencesLength' n = (^ n) 2

testTheorem4 :: Int -> Property
testTheorem4 n = n >= 0 ==> subsequencesLength n == subsequencesLength' n

genMax :: Gen Int
genMax = (arbitrary :: Gen Int) `suchThat` (<= 25)

exercise2 :: IO ()
exercise2 = do
  quickCheck $ forAll genMax testTheorem4