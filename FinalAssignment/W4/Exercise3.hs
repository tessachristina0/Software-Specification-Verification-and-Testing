-- SSVT Haskell Lab
-- Week 4 - Group 5
-- Exercise 3: 
-- Deliverables: 
-- Time spend: ?? minutes --
import Data.List
import System.Random
import Test.QuickCheck

import FinalAssignment.W4.LTS

-- straces :: IOLTS -> [Trace]
-- straces (_, _, _, labeledTrans, _) = map (\(_, x, _) -> x) (filter straces' labeledTrans)

-- straces' :: [LabeledTransition] -> State -> Trace -> [Trace] -> [Trace]
-- straces' transitions curState curTrace allTraces = allTraces ++ [curTrace] ++ map (\(_, label, s2) -> [label] ++ (straces' transitions s2 curTrace allTraces)) (finAllPossibleTraces transitions curState)

findAllPossibleTransitions :: [LabeledTransition] -> TraceState -> TraceState
findAllPossibleTransitions transitions = concatMap (\(state, trace) -> map (\(_, label, s2) -> (s2, trace ++ [label])) $ filter (\(s1, _, _) -> s1 == state) transitions)

type TraceState = [(State, Trace)]



start :: IOLTS -> [Trace]
start (_, _, _, transitions, s) = aaa transitions []


aaa :: [LabeledTransition] -> TraceState -> [Trace]
aaa _ [] = []
aaa transitions memory =
    map (\(_, trace) -> trace) memory ++ concatMap (\transition -> aaa transitions [transition]) (findAllPossibleTransitions transitions memory)


-- map (\(_, trace) -> trace) memory : concatMap (\(_, label, s2) -> aaa transitions memory s2 (curTrace ++ [label])) (finAllPossibleTransitions transitions curState)
-- sortBy (\(x1, _, x2) (y1, _, y2) -> compare (x1 /= x2) (y1 /= y2)  )

-- 1. start with start state
-- 2. find all possible labels
-- 3. Append all transition labels 

-- S-trace toevoegen aan input
