-- SSVT Haskell Lab
-- Week 4 - Group 5
-- Exercise 1: The IOLTS datatype allows, by definition, for the creation of IOLTS's that are not valid. 
-- Make a list of factors that result in invalid IOLTS's. Write a function validateLTS :: IOLTS -> Bool that 
-- returns true iff a given LTS is valid according to the definition given in the Tretmans paper.
-- Deliverables: list of factors, implementation, short test report, indication of time spent.
-- Time spend: ?? minutes --
import Data.List
import Test.QuickCheck
import Donovan.Week4.LTS

-- Make a list of factors 
-- 1. q (the states) are a countable, non-empty set
-- 2. l (the labels) are a countable set
-- 3. lt contains all of the unique transitions 
-- 4. q0 is the initial state

validateLTS :: IOLTS -> Bool
validateLTS (states, labelsI, labelsU, labeledTransitions, startState) 
    | states /= [] = True
    | checkForDuplicates labelsI labelsU labeledTransitions = True
    | notElem tau (labelsI ++ labelsU) = True
    | elem startState states = True
    | intersect (labelsI) (labelsU) == [] = True
    | all (checkValidTransition states (tau : (labelsI ++ labelsU))) labeledTransitions = True

checkValidTransition :: [State] -> [Label] -> LabeledTransition -> Bool
checkValidTransition states labels (startState, label, nextState) 
    | elem startState states = True
    | elem nextState states = True
    | elem label labels = True

checkForDuplicates :: (Eq a1, Eq a2, Eq a3) => [a1] -> [a2] -> [a3] -> Bool
checkForDuplicates labelI labelU labeledTransitions  
    | labelI == nub labelI = True
    | labelU == nub labelU = True
    | labeledTransitions == nub labeledTransitions = True
