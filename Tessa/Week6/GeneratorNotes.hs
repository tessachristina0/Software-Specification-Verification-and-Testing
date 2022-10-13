-- Generator to create a maximum number of states 
genStates :: Gen Integer
genStates = do arbitrary `suchThat` (<=6)

-- Generator for random transitions
genTransitions :: Gen [LabeledTransition]
genTransitions = arbitrary :: Gen [LabeledTransition]

ltsGen :: Gen IOLTS
ltsGen = do 
    n <- genTransitions
    return $ if (length n == 0) then ([],[],[],[],0) else createIOLTS n


-- Function to validate an LTS by checking all its conditions.
-- Then a boolean is returned which says if the LTS is valid or isn't valid.
validateLTS (states, labelsI, labelsU, labeledTransitions, startState) 
    | states /= [] = True
    | checkForDuplicates labelsI labelsU labeledTransitions = True
    | notElem tau (labelsI ++ labelsU) = True
    | elem startState states = True
    | intersect (labelsI) (labelsU) == [] = True
    | all (checkValidTransition states (tau : (labelsI ++ labelsU))) labeledTransitions = True


-- 3) Use your generator(s) to test the function that was created in the previous exercise.
main :: IO ()
main = do
    quickCheck $ forAll ltsGen (\lts -> validateLTS lts == True)

