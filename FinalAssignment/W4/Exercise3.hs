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

straces' :: IOLTS -> TraceState -> [Trace]
straces' _ [] = []
straces' iolts@(_, _, _, transitions, _) memory =
    map snd memory ++ straces' iolts (findAllPossibleTransitions transitions memory)

straces :: IOLTS -> [Trace]
straces iolts@(_, _, _, transitions, s) = nub $ straces' (addQuiescenseSteps iolts) [(s, [])]


-- genTraces :: Gen [Trace]
-- genTraces = do
--     l <- vectorOf 100 ltsGen
--     sublistOf $ take 100 (straces (generate $ ltsGen))

