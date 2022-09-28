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

-- How do i check if all of the [(x, _, _)] from the tuples are present in the [q] this is what i don't know :')
tuplesToList :: [(a, a, a)] -> [a]
tuplesToList _ = []
tuplesToList ((a, b, c):xs) = a : tuplesToList xs

validateLTS :: IOLTS -> Bool
validateLTS _ = False
validateLTS ([], [l1], [l2], [lt], q0) = False
validateLTS ([q], [l1], [l2], [lt], q0) = not (null [q])
validateLTS ([q], [l1], [l2], [lt], q0) = not (null [l1]) && not (null [l2])
validateLTS ([q], [l1], [l2], [(x, _, _)], s) = undefined
validateLTS ([q], [l1], [l2], [lt], q0) = q0 == 0 || q0 == 1

-- validateLTS ([q], [l1], [l2], [lt], s) 
--             | [q] == [State] = True
--             | not (null [q]) = False
--             | not (null [l1]) && not (null [l2]) = False
--             | otherwise = True