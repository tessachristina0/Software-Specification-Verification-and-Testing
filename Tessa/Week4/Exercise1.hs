-- SSVT Haskell Lab
-- Week 4 - Group 5
-- Exercise 1: The IOLTS datatype allows, by definition, for the creation of IOLTS's that are not valid. 
-- Make a list of factors that result in invalid IOLTS's. Write a function validateLTS :: IOLTS -> Bool that 
-- returns true iff a given LTS is valid according to the definition given in the Tretmans paper.
-- Deliverables: list of factors, implementation, short test report, indication of time spent.
-- Time spend: 4 hours --
module Exercise1 where
import Data.List
import Test.QuickCheck
import LTS

-- Make a list of factors 
-- 1. States, labels and labeled transitions list do have duplicates
-- 2. The start state isn't part of the states list
-- 3. The union of input and output labels contains tau
-- 4. Values of the input label are present in the output label (intersection)
-- 5. There is a transition which has a state that isn't part of the states list
-- 6. There is a label in a transation which is not present in the union of the input and output labels

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


exercise1 :: IO ()
exercise1 = do
  putStrLn "\bExercise 1\nTime spent +/- 4 hours\n"
  putStrLn "tretmanK1: "
  print (validateLTS tretmanK1)
  putStrLn "\ntretmanK2: "
  print (validateLTS tretmanK2)
  putStrLn "\ntretmanK3: "
  print (validateLTS tretmanK3)
  putStrLn "\ntretmanI1: "
  print (validateLTS tretmanI1)
  putStrLn "\ntretmanI2: "
  print (validateLTS tretmanI2)
  putStrLn "\ntretmanI3: "
  print (validateLTS tretmanI3)
  putStrLn "\ntretmanI4: "
  print (validateLTS tretmanI4)
  putStrLn "\n"
