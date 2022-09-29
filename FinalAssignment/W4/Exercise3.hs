-- SSVT Haskell Lab
-- Week 4 - Group 5
-- Exercise 3: 
-- Deliverables: 
-- Time spend: ?? minutes --
import Data.List
import System.Random
import Test.QuickCheck

import FinalAssignment.W4.LTS

type TraceState = [(State, Trace)]

filterInputSteps :: IOLTS -> [LabeledTransition]
filterInputSteps (states, input, output, lts, s) = filter (\(s1, label, _) -> label `elem` input) lts

createQuiescenseSteps :: IOLTS -> [LabeledTransition]
createQuiescenseSteps iolts@(states, input, output, lts, s) = nub (lts ++ map (\(s1, label, _) -> (s1, label, s1)) (filterInputSteps iolts))

addQuiescenseSteps :: IOLTS -> IOLTS
addQuiescenseSteps iolts@(states, input, output, lts, s) = (states, input, output, createQuiescenseSteps iolts, s)

findAllPossibleTransitions :: [LabeledTransition] -> TraceState -> TraceState
findAllPossibleTransitions transitions = concatMap (\(state, trace) -> map (\(_, label, s2) -> (s2, trace ++ [label])) $ filter (\(s1, _, _) -> s1 == state) transitions)

aaa :: [LabeledTransition] -> TraceState -> [Trace]
aaa _ [] = []
aaa transitions memory =
    map snd memory ++ aaa transitions (findAllPossibleTransitions transitions memory)

start :: IOLTS -> [Trace]
start (_, _, _, transitions, s) = aaa transitions [(s, [])]

straces :: IOLTS -> [Trace]
straces iolts = nub $ start $ addQuiescenseSteps iolts