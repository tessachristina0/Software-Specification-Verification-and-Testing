-- SSVT Haskell Lab
-- Week 4 - Group 5
-- Exercise 1: The IOLTS datatype allows, by definition, for the creation of IOLTS's that are not valid. 
-- Make a list of factors that result in invalid IOLTS's. Write a function validateLTS :: IOLTS -> Bool that 
-- returns true iff a given LTS is valid according to the definition given in the Tretmans paper.
-- Deliverables: list of factors, implementation, short test report, indication of time spent.
-- Time spend: - hours --
module Donovan.Week4.Exercise4 where
import Data.List
import Test.QuickCheck
import Donovan.Week4.LTS


exercise4 :: IO ()
exercise4 = do
  putStrLn "\bExercise 1\nTime spent +/- - hours\n"
  putStrLn "\n"
