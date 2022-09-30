module FinalAssignment.W4.Exercise2 where

-- SSVT Haskell Lab
-- Week 4 - Group 5
-- Exercise 2: 
-- Deliverables: Random IOLTS generator(s), QuickCheck tests for validateLts, indication of time spent
-- Time spend: ?? minutes --
import Data.List
import System.Random
import Test.QuickCheck
import FinalAssignment.W4.LTS
import FinalAssignment.W4.Exercise1

-- 1) Implement at least one random generator ltsGen :: Gen IOLTS for Labelled Transition Systems. 

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

-- 2) You may also implement additional generators to generate LTS/IOLTS's with certain properties. 

-- Invalid IOLTSs due to removing the first state. Any IOLTS without its first state does not make sense. 
ltsGenInv :: Gen IOLTS
ltsGenInv = do 
    (a,b,c,d,e) <- ltsGen
    return $ ((tail a),b,c,d,e)

-- 3) Use your generator(s) to test the function that was created in the previous exercise.
main :: IO ()
main = do
    quickCheck $ forAll ltsGen (\lts -> validateLTS lts == True)


-- NOTES:
-- Older function: needed random transitions and random labels
-- createTransitions :: [Label] -> Integer -> [LabeledTransition]
-- createTransitions ls n = [(s1, l, (s1 + t)) | s1 <- [1..n], l <- ls, t <- [1..(n-s1)]]

-- STEPS: generate random string
-- generate (arbitrary :: Gen String)
-- reuse random labels in recursive function: save them in list
-- use freq to reuse old one or new label
-- limit string length OR let quickcheck run wild (will start small)
