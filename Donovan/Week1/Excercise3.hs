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

-- Excercise 3
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) 
    where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

quicktest5 :: Int -> Property
quicktest5 n = n >= 0 ==> length (perms [1..n]) == length (permutations [1..n])

genMax :: Gen Int
genMax = (arbitrary :: Gen Int) `suchThat`(<= 9)

main :: IO()
main = do
  quickCheck $ forAll genMax quicktest5
  putStrLn "Hooray"