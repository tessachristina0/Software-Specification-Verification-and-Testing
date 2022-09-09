-- Author: Donovan Schaafsma
import Data.List
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    forAll,
    quickCheck,
    suchThat,
    (==>),
  )

finiteSetA :: Int -> Int
finiteSetA n = length (subsequences [1..n])

finiteSetPA :: Int -> Int
finiteSetPA n = (^n) 2

-- QuickCheck test
prop_alwaysEqualLength :: Int -> Property
prop_alwaysEqualLength n = n >= 0 ==> finiteSetA n == finiteSetPA n

genMax :: Gen Int
genMax = (arbitrary :: Gen Int) `suchThat` (<= 25)

main :: IO ()
main = do
  quickCheck $ forAll genMax prop_alwaysEqualLength