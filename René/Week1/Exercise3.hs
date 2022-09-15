import Data.List (permutations)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    forAll,
    quickCheck,
    suchThat,
    (==>),
  )

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (insrt x) (perms xs))
  where
    insrt x [] = [[x]]
    insrt x (y : ys) = (x : y : ys) : map (y :) (insrt x ys)

testTheorem5 :: Int -> Property
testTheorem5 n = n >= 0 ==> length (perms [1 .. n]) == length (permutations [1 .. n])

genMax :: Gen Int
genMax = (arbitrary :: Gen Int) `suchThat` (<= 9)

exercise3 :: IO ()
exercise3 = do
  quickCheck $ forAll genMax testTheorem5