-- Donovan Schaafsma
-- Week 2 - Group 5
-- Exercise 5
module Exercise5 where

import Data.List (permutations)
import Test.QuickCheck (Property, quickCheck, (==>))

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

{-
This was a very hard assignment since we were contemplating multiple ways of implementing derangement. We were first looking for ways to recursively compare lists to one another until a correct derangement was found.
However this approach became complicated very quickly. This function takes two lists and compares if all the values are elements of each list if the indexes of the values are equal to one another. In a derangement
it is not allowed that a values keeps the same index.
-}
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

-- This function creates a derangement list for a given N amount of integers.
deran :: Int -> [[Int]]
deran n = filter (\x -> isDerangement x [0 .. n -1]) (permutations [0 .. n -1])

-- This test tests the property of the length of the derangedlist. When a normal list is deranged, it should remain the same length. 
testPropertyLength :: [Int] -> [Int] -> Bool
testPropertyLength normalList derangedList = isDerangement normalList derangedList --> (length normalList == length derangedList)

-- This test tests the property validity of the derangement. In the exercise5 method, the correct integer list combination is given for comparison, the method should return True if the parameters implemented correctly.
testPropertyDerangementCorrectIntegerList :: [Int] -> [Int] -> Bool
testPropertyDerangementCorrectIntegerList = isDerangement

-- This test tests the property validity of the derangement. In the exercise5 method, the incorrect integer list combination is given for comparison, the method should return False if the parameters implemented correctly.
testPropertyDerangementIncorrectIntegerList :: [Int] -> [Int] -> Bool
testPropertyDerangementIncorrectIntegerList = isDerangement

exercise5 :: IO ()
exercise5 = do
  putStrLn "\bTest the length property"
  print (testPropertyLength [1, 2, 3] [2, 3, 1])
  putStrLn "\n"

  putStrLn "\bTest the derangement function with a correct list"
  print (testPropertyDerangementCorrectIntegerList [0, 1, 2, 3] [2, 0, 3, 1])
  putStrLn "\n"

  putStrLn "\bTest the derangement function with an incorrect list"
  print (testPropertyDerangementIncorrectIntegerList [0, 1, 2, 3] [2, 0, 1, 3])
  putStrLn "\n"

  putStrLn "Time indication: 3 hours"