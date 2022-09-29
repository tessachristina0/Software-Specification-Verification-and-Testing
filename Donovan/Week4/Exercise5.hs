-- SSVT Haskell Lab
-- Week 4 - Group 5
-- Exercise 5: Use your after function to implement the functions out and ioco (infix) corresponding with 
-- the definitions in the Tretmans paper. Create tests to test the validity of your implementations.
-- Deliverables: Haskell program, tests, short test report, indication of time spent.
-- Time spend: - hours --
module Donovan.Week4.Exercise5 where
import Data.List 
import Test.QuickCheck
import Donovan.Week4.LTS
import Donovan.Week4.Exercise4

out :: IOLTS -> [State] -> [Label]
out (states, labelI, labelU, transitions, startState) state = [label | (firsState, label, _) <- transitions, firsState == head state]

exercise5 :: IO ()
exercise5 = do
  putStrLn "\bExercise 1\nTime spent +/- - hours\n"
  putStrLn "Testing same functions for the out function as in the slides: "
  putStrLn "coffeeImpl1 for after on coin : "
  print (out coffeeImpl1 (coffeeImpl1 `after` ["coin"]))
  putStrLn "coffeeModel1 for after on coin : "
  print (out coffeeModel1 (coffeeModel1 `after` ["coin"]))
  putStrLn "\n"