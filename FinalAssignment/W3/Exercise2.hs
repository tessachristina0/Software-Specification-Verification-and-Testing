-- SSVT Haskell Lab
-- Week 3 - Group 5
-- Exercise 2: Test the parse function
-- Deliverables: test report describing the test method used and the outcome of the test, indication of time spent.
-- Time spend: 30 minutes --
module FinalAssignment.W3.Exercise2 where

import FinalAssignment.W3.Lecture3
import Test.QuickCheck
import FinalAssignment.W3.Exercise4
import Test.QuickCheck.Property
import Test.QuickCheck.Gen

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Test output is a list, the output of the parse function should be a list
prop_outputIsAList :: String -> Bool
prop_outputIsAList s = show (parse s) == "[" ++ s ++ "]"

-- Test empty print, an empty string should return a empty list
prop_emptyParse :: Bool
prop_emptyParse = [] == parse ""

-- The given input should return the given output
prop_parseRight :: (Form, String) -> Bool
prop_parseRight (f, s) = [f] == parse s

-- TODO: Gebruik deze in combinatie met de generator van Tessa als die af is
-- We can use show on form to generate a string of a form. 
-- Combining this with a generator, we can generate infinite forms to test with the parse function.
prop_parseRight' :: Form -> Bool
prop_parseRight' f = [f] == parse (show f)
forms =
  [ (Prop 1, "1"),
    (Neg (Prop 1), "-1"),
    (Cnj [Impl (Neg (Prop 22)) (Neg (Prop 21)), Prop 3], "*((-22==>-21) 3)"),
    (Equiv (Prop 2) (Neg (Dsj [Neg (Prop 14), Prop 6])), "(2<=>-+(-14 6))"),
    (Dsj [Cnj [Neg (Prop 7), Prop 0], Neg (Prop 9)], "+(*(-7 0) -9)")
  ]

exercise2 :: IO ()
exercise2 = do
    putStrLn "\bExercise 3\nTime spent +/- 30 minutes\n"
    putStrLn ("The output of the parse function should be a list: " ++ show (prop_outputIsAList "*((-22==>-21) 3)"))
    putStrLn ("An empty string should return a empty list: " ++ show prop_emptyParse)
    putStrLn ("The given input should return the given output:" ++ show (forall forms prop_parseRight))
    -- TODO: Gebruik deze in combinatie met de generator van Tessa als die af is
    -- quickCheck $ forAll (genForm 10) prop_parseRight'


