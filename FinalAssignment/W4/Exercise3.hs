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

finAllPossibleTraces :: [LabeledTransition] -> State -> [LabeledTransition]
finAllPossibleTraces transitions forState = filter (\(s1, _, _) -> s1 == forState ) transitions


-- bbb transitions curState curTrace = (concatMap (\(_, label, s2) -> bbb transitions s2 (curTrace ++ [label])) (finAllPossibleTraces transitions curState))

bbb :: IOLTS -> [Trace]
bbb (_, _, _, transitions, s) = aaa transitions (finAllPossibleTraces transitions s) s [""]


aaa :: [LabeledTransition] -> [LabeledTransition] -> State -> Trace -> [Trace]
aaa transitions possibleTransitions curState curTrace = 
    curTrace : concatMap (\(_, label, s2) -> aaa transitions (finAllPossibleTraces transitions s2) s2 (curTrace ++ [label]) ) possibleTransitions



-- 1. start with start state
-- 2. find all possible labels
-- 3. Append all transition labels 

