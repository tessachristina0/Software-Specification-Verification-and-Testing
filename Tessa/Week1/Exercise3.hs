import Data.List

module Exercise3 where 
import Test.QuickCheck

-- Factorial function from StackOverflow..*
fact n = if n == 0 then 1 else n * fact(n-1)
fact n = foldl(*) 1 [1..n]
fact n = product [1..n]


perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)


perms' :: Int -> Int
perms' = 
