module FinalAssignment.W4.Exercise3 where

-- SSVT Haskell Lab
-- Week 4 - Group 5
-- Time spend: 5 hours --
import Data.List
import System.Random
import Test.QuickCheck

import FinalAssignment.W4.LTS
import FinalAssignment.W4.Exercise2

type TraceState = [(State, Trace)]

-- Retrieve all input steps
filterInputSteps :: IOLTS -> [LabeledTransition]
filterInputSteps (states, input, output, lts, s) = filter (\(s1, label, _) -> label `elem` input) lts

-- Create Quiescense Steps for all input steps
createQuiescenseSteps :: IOLTS -> [LabeledTransition]
createQuiescenseSteps iolts@(states, input, output, lts, s) = nub (lts ++ map (\(s1, label, _) -> (s1, label, s1)) (filterInputSteps iolts))

-- Add Quiescense Steps
addQuiescenseSteps :: IOLTS -> IOLTS
addQuiescenseSteps iolts@(states, input, output, lts, s) = (states, input, output, createQuiescenseSteps iolts, s)

-- Find all possible next transitions for a trace
findAllPossibleTransitions :: [LabeledTransition] -> TraceState -> TraceState
findAllPossibleTransitions transitions = concatMap (\(state, trace) -> map (\(_, label, s2) -> (s2, trace ++ [label])) $ filter (\(s1, _, _) -> s1 == state) transitions)

-- Search breadth first
straces' :: IOLTS -> TraceState -> [Trace]
straces' _ [] = []
straces' iolts@(_, _, _, transitions, _) memory =
    map snd memory ++ straces' iolts (findAllPossibleTransitions transitions memory)

straces :: IOLTS -> [Trace]
straces iolts@(_, _, _, transitions, s) = nub $ straces' (addQuiescenseSteps iolts) [(s, [])]

-- Generator
genTraces :: Gen [Trace]
genTraces = do
    lv <- ltsGen
    sublistOf $ take 100 $ straces lv

-- We were not able to create any tests due to time constraint. 
-- The exercise already took us 5 hours.
-- What we could've tested are some corner models.

exercise3 :: IO ()
exercise3 = do
    putStrLn "\ncoffeeModel6:"
    print coffeeModel6
    putStrLn "\nstraces coffeeModel6:"
    print $ take 10 (straces coffeeModel6)

    putStrLn "\ntretmanK1:"
    print tretmanK1
    putStrLn "\nstraces tretmanK1:"
    print $ take 10 (straces tretmanK1)

    putStrLn "\ntretmanS2:"
    print tretmanS2
    putStrLn "\nstraces tretmanS2:"
    print $ take 10 (straces tretmanS2)