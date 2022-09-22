-- SSVT Haskell Lab
-- Week 3 - Group 5
-- Exercise 1: Give the definitions of a contradiction, tautology, entailment and equivalence.
-- Deliverables: implementation, description of your method of checking the definitions, indication of time spent.
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Example from the slides
satisfiable :: Form -> Bool
satisfiable f = any (`evl` f) (allVals f)

-- Contradiction
contradiction :: Form -> Bool
contradiction f = not (any (`evl` f) (allVals f))

-- A formula f is a tautology if it is satisfied by all valuations. - From the slides
tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

-- | logical entailment - B logically entails A is true if and only if all the valuations that satisfy B also satisfy A. - From the slides
entails :: Form -> Form -> Bool
entails f1 f2 = all (`evl` Impl f1 f2) (allVals (Impl f1 f2))

-- | logical equivalence - A and B are equivalent - From the slides
equiv :: Form -> Form -> Bool
equiv f1 f2 = all (`evl` Equiv f1 f2) (allVals (Equiv f1 f2))


-- Tests:
-- Test of the contradiction, showed in a tuple per formula
testContradiction :: ((String, Bool), (String, Bool), (String, Bool))
testContradiction = (("form 1", contradiction (Neg form1)), ("form 2", contradiction (Neg form2)), ("form 3", contradiction (Neg form3)))

-- Test of the tautology, showed in a tuple per formula
testTautology :: ((String, Bool), (String, Bool), (String, Bool))
testTautology = (("form 1", tautology form1 ), ("form 2", tautology form2), ("form 3", tautology form3))

-- Test of the entailment, showed in a tuple per formula
testEntails :: ((String, Bool), (String, Bool), (String, Bool))
testEntails = (("form 1, form 2", entails form1 form2 ), ("form 1, form 3", entails form1 form3), ("form 2, form", entails form2 form3))

-- Test of the equivalence, showed in a tuple per formula
testEquiv :: ((String, Bool), (String, Bool), (String, Bool))
testEquiv = (("form 1, form 2", equiv form1 form2 ), ("form 1, form 3", equiv form1 form3), ("form 2, form", equiv form2 form3))

exercise1 :: IO ()
exercise1 = do
  putStrLn "\bExercise 1\nTime spent +/- 50 minutes\n"
  putStrLn "Contradiction: "
  print testContradiction
  putStrLn "\nTautology: "
  print testTautology
  putStrLn "\nEntails: "
  print testEntails
  putStrLn "\nEquiv: "
  print testEquiv
  putStrLn "\n"