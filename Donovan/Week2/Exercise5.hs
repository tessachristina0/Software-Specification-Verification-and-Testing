-- Donovan Schaafsma
-- Week 2 - Group 5
-- Exercise 5
module Exercise5 where

import Data.List (delete, permutations, sortBy)

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement normalList derangedList =
  and
    [ x `elem` derangedList
        && (index x normalList /= index x derangedList)
      | x <- normalList
    ]
  where
    index n (x : xs)
      | n == x = 0
      | otherwise = 1 + index n xs

deran :: Int -> [[Int]]
deran n = filter (\x -> isDerangement x [0..n-1]) (permutations [0..n-1])