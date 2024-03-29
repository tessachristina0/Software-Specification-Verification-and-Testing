-- SSVT Haskell Lab
-- Week 4 - Group 5
-- Exercise 4: Implement the function after (infix) for IOLTS corresponding with the definition in the Tretmans paper. 
-- Create tests to test the validity of your implementation.
-- Deliverables: Haskell program, tests, short test report, indication of time spent.
-- Time spend: 3 hours --
module Donovan.Week4.Exercise4 where
import Data.List
import Test.QuickCheck
import Donovan.Week4.LTS

-- Function to create an infix after function with the after functionalities from the paper
-- This function returns a list of states
infix `after`
after :: IOLTS -> Trace -> [State]
after (states, labelI, labelU, transition, startState) = returnNextState transition [startState]

-- Function to decide what the next state is after an inserted startState
-- This function returns a list of states
returnNextState :: [LabeledTransition] -> [State] -> Trace -> [State]
returnNextState transition startState trace
    | null trace = startState
    | not $ null trace = returnNextState transition (getNextState transition startState trace) (tail trace)

-- Function to get the next state inside a list
-- This function returns a list of states
getNextState :: [LabeledTransition] -> [State] -> Trace -> [State]
getNextState transition startState trace = [nextState | (firstState, label, nextState) <- transition, 
                                                        label == head trace
                                                        && elem firstState startState]
exercise4 :: IO ()
exercise4 = do
  putStrLn "\bExercise 1\nTime spent +/- 3 hours\n"
  putStrLn "coffeeImpl1 coin and coffee: "
  print (coffeeImpl1 `after` ["coin", "coffee"])
  putStrLn "\ncoffeeImpl1 coin: "
  print (coffeeImpl1 `after` ["coin"])
  putStrLn "\ncoffeeImpl1 coffee: "
  print (coffeeImpl1 `after` ["coffee"])
  putStrLn "\ncoffeeImpl2 coin and coffee: "
  print (coffeeImpl2 `after` ["coin", "coffee"])
  putStrLn "\ncoffeeImpl2 coin: "
  print (coffeeImpl2 `after` ["coin"])
  putStrLn "\ncoffeeImpl2 coffee: "
  print (coffeeImpl2 `after` ["coffee"])
  putStrLn "\ncoffeeImpl3 coin and coffee: "
  print (coffeeImpl3 `after` ["coin", "coffee"])
  putStrLn "\ncoffeeImpl3 coin: "
  print (coffeeImpl3 `after` ["coin"])
  putStrLn "\ncoffeeImpl3 coffee: "
  print (coffeeImpl3 `after` ["coffee"])
  putStrLn "\n"
