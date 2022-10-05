-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 2: Write a function countSurvivors that counts the number of survivors.
-- Deliverables: implementation, documentation of approach, effect of using different mutators/properties, indication of time spent.
-- Time spend: ~ hours --
module FinalAssignment.W5.Haskell.Exercise2 where

import FinalAssignment.W5.Haskell.MultiplicationTable
import FinalAssignment.W5.Haskell.Mutation
import Test.QuickCheck
import Data.Maybe

allMutators = [addElements, removeElements]

countSurvivors :: Int -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [Gen [Maybe Bool]]
countSurvivors nrOfMutants props fn =
  [ vectorOf nrOfMutants (mutate mutator prop fn 3) | prop <- props, mutator <- allMutators]


aa = do
  x <- bb
  let y =  length $ filter (\x -> x == Just False) (concat x)
  return y

-- count = tests >>= \x -> return $ length $ filter (== Just True) x
bb :: Gen [[Maybe Bool]]
bb = sequence $ countSurvivors 10 [prop_tenElements] multiplicationTable

-- Voert alle props uit in combinatie met mutator
-- Dan voeg je de lijsten samen door middel een lijst te maken van alle 1en

exercise2 :: IO ()
exercise2 = do
  putStrLn "\bExercise 1\nTime spent +/- ~ hours\n"
  putStrLn "\n"
