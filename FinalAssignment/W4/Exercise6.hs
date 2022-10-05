-- SSVT Haskell Lab
-- Week 4 - Group 5
-- Exercise 6: Write a function testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool that returns True if the SUT 
-- correctly implements the IOLTS and either returns false or throws an error if it doesn't. 
-- Deliverables: Haskell program, description of each bug, indication of time spent.
-- Time spend: 360 minutes --
import Data.List
import System.Random
import Test.QuickCheck
import LTS

        
-- Door implementations: Door 1 is correct
doorImpl1 :: State -> Label -> (State, Label)
doorImpl1 0 "close" = (1, "closed")
doorImpl1 1 "open" = (0, "opened")
doorImpl1 1 "lock" = (2, "locked")
doorImpl1 2 "unlock" = (1, "unlocked")
doorImpl1 _ _ = error "Invalid command and/or state!"
-- Note: States returned by the door implementation are internal states, and do not necessarily correspond with states in your model

-- Each time the door is used, the previously returned state must be passed as the State argument
-- The initial state for each of the door implementations is 0
-- Create an IOLTS that specifies the correct behavior for this door.


-- IOLST according to door implementation 1
validIOLTS :: IOLST
validIOLTS = createIOLTS [ (0, "?close", 1), (1, "!closed", 2), (1, "!unlocked", 3), (2, "?lock", 5), (2, "?open", 4), 
                           (3, "?lock", 5), (3, "?unlock", 6), (4, "!opened", 0), (5, "!locked", 6), (6, "?unlock", 2),
                           (6, "?unlock", 3)]

-- Start of the test
-- Return the next transition
getTransition :: [LabeledTransition] -> Label -> State -> LabeledTransition
getTransition [] _ _ = (-1,"error",-1)
getTransition ((s1, x, s2):ts) l s | s1 == s && x == l   = (s1, x, s2)
                                   | otherwise           = getTransition ts l s

getNextTrans :: [LabeledTransition] -> State -> [LabeledTransition]
getNextTrans [] s = []
getNextTrans ((s1,x,s2):ts) s | s1 == s   = [(s1, x, s2)] ++ getNextTrans ts s
                              | otherwise = getNextTrans ts s
-- TODO: check for failure (= return of []) and get Transitions out of list as input for door


-- TODO: check if state in returned (State,Label) is 0. Then quit test.

nextState :: [LabeledTransition] -> State
nextState [] = -1
nextState ((x, l, y):ts) = y

-- Function to start off checking the implementation by making a list of all the output (State, Label) tuples resulting
-- from the start input (0, "close") which is accurate for every door-implementatiton.
startCheck :: [LabeledTransition] -> State -> (State -> Label -> (State, Label)) -> [(State, Label)]
startCheck lts start f = case (sx, ls, sy) of 
    (_, "error", _) -> [(-1, "error")]
    otherwise       -> case getNextTrans lts s2 of
        []        -> [(-1, "error")] -- no next states!
        otherwise -> case f (nextState ((s3, l2, s4):ts)) l2 of 
            -1        -> [(-1, "error")]
            otherwise -> [f (nextState ((s3, l2, s4):ts)) l2] ++ [f (nextState ts) l2]
    where 
        (n, l) = f 0 "close"
        (sx, ls, sy) = getTransition lts "close" start
        (s1, l1, s2) = getTransition lts l sy
        ((s3, l2, s4):ts) = getNextTrans lts s2

-- The output list is then used to check each of the resulting tuples in the model, by checking the previous output-state
-- with the implementation-output label
testLTS :: [LabeledTransition] -> (State -> Label -> (State, Label)) -> [(State, Label)] -> Bool 
testLTS lts f [] = False -- There are no other stimuli/responses in implementation!
testLTS lts f (x:xs) = testLTS' lts f x && testLTS lts f xs

-- This function takes one of the tuples and, if accurate, recursively checks the next transition
testLTS' :: [LabeledTransition] -> (State -> Label -> (State, Label)) -> (State, Label) -> Bool
testLTS' lts f (n, l) = case (s1, l1, s2) of
    (_, "error", _) -> False
    otherwise       -> case (s3, l2, s4) of
        (_, "error", _) -> False
        otherwise       -> testDoor (getNextTrans lts s2) f n
    where 
        (s1, l1, s2) = getTransition lts l n
        ((s3, l2, s4):ts) = getNextTrans lts s2

-- This function is used to recursively check all transitions with the same start-state in the model
testDoor :: [LabeledTransition] -> [LabeledTransition] -> (State -> Label -> (State, Label)) -> State -> Bool
testDoor lts [] door n = True
testDoor lts ((s1, x, s2):xs) door n | n2 == 0              = True && testDoor xs door n -- Path returns to 0
                                     | otherwise            = testLTS' lts door (n2, l2) && testDoor xs door n && True
                                     where
                                        (n2, l2) = door n x

-- The main test for the IOLST against the SUT model
testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT (_, _, _, lts, start) door | startCheck lts start door !! 1 == (-1, "error") = False
                                             | otherwise                                       = testLTS lts door (startCheck lts start door)

-- Think about a way to show descriptive errors, such that it is apparent what the flaw is and how to fix it.
-- NOTE: Functions do not work properly due to syntax errors and lack of time fixing this. Hopefully, the way of thinking is visible.