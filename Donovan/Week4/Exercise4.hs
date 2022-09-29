-- SSVT Haskell Lab
-- Week 4 - Group 5
-- Exercise 4: Implement the function after (infix) for IOLTS corresponding with the definition in the Tretmans paper. 
-- Create tests to test the validity of your implementation.
-- Deliverables: Haskell program, tests, short test report, indication of time spent.
-- Time spend: - hours --
module Donovan.Week4.Exercise4 where
import Data.List
import Test.QuickCheck
import Donovan.Week4.LTS

returnNextState :: [LabeledTransition] -> [State] -> Trace -> [State]
returnNextState transition startState trace
    | null trace = startState
    | not $ null trace = returnNextState transition (getNextState transition startState trace) (tail trace)

getNextState :: [LabeledTransition] -> [State] -> Trace -> [State]
getNextState transition startState trace = [nextState | (firstState, label, nextState) <- transition, label == head trace && elem firstState startState]

after :: IOLTS -> Trace -> [State]
after (_, _, _, transition, startState) = returnNextState transition [startState]


exercise4 :: IO ()
exercise4 = do
  putStrLn "\bExercise 1\nTime spent +/- - hours\n"
  putStrLn "\n"
