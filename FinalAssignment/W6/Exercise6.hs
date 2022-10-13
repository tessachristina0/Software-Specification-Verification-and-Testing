{-# LANGUAGE InstanceSigs #-}

-- SSVT Haskell Lab
-- Week 6 - Group 5
-- Exercise 6: Test the functions symClos and trClos from the previous exercises.
-- Deliverables: test code, short test report, indication of time spent.
-- Time spend: 2 hour --

module FinalAssignment.W6.Exercise6 where

import Data.List
import FinalAssignment.W6.Exercise3 (symClos)
import FinalAssignment.W6.Exercise5 (trClos)
import Test.QuickCheck

type Rel a = [(a, a)]

newtype Int' = Int' Int deriving (Show, Eq, Ord)

instance Arbitrary Int' where
  arbitrary :: Gen Int'
  arbitrary = oneof $ map (return . Int') [1 .. 8]

-- TRANSITIVE CLOSURE OF A RELATIONSHIP
isTransitive :: Eq a => Rel a -> Bool
isTransitive rel = and [ (a1, b2) `elem` rel | (a1, a2) <- rel, 
                                               (b1, b2) <- rel, 
                                               a2 == b1 ]

-- All relations should be transitive
prop_relationsAreTransitive :: Rel Int' -> Bool
prop_relationsAreTransitive rel = isTransitive $ trClos rel

-- The intersection of two transitive relations is transitive.
-- See https://en.wikipedia.org/wiki/Transitive_closure#Properties
prop_intrsectIsTransitive :: Rel Int' -> Rel Int' -> Bool
prop_intrsectIsTransitive rel1 rel2 = isTransitive $ trClos (rel1 `intersect` rel2)

-- The union of two transitive relations need not be transitive.
-- To preserve transitivity, one must take the transitive closure.
-- This occurs, for example, when taking the union of two equivalence relations.
-- To obtain a new equivalence relation take the transitive closure
-- (reflexivity and symmetry—in the case of equivalence relations—are automatic)
-- See https://en.wikipedia.org/wiki/Transitive_closure#Properties
prop_unionIsNotTransitive :: Rel Int' -> Rel Int' -> Bool
prop_unionIsNotTransitive rel1 rel2 = isTransitive $ trClos (trClos rel1 `union` trClos rel2)

-- Transitive Closure should be sorted
prop_trClosIsSorted :: Rel Int' -> Bool
prop_trClosIsSorted rel = sort trClos' == trClos'
  where
    trClos' = trClos rel

-- SYMMETRIC CLOSURE OF A RELATIONSHIP
isSymmetric :: Eq a => Rel a -> Bool
isSymmetric rel = and [(b, a) `elem` rel | (a, b) <- rel]

-- All relations should be symmetric
prop_relationsAreSymmetric :: Rel Int' -> Bool
prop_relationsAreSymmetric rel = isSymmetric $ symClos rel

-- Symmetric closure should be sorted
prop_symClosIsSorted :: Rel Int' -> Bool
prop_symClosIsSorted rel = sort symClos' == symClos'
  where
    symClos' = symClos rel

exercise6 :: IO ()
exercise6 = do
  putStrLn "All relations should be transitive:"
  quickCheck prop_relationsAreTransitive

  putStrLn "\nThe intersection of two transitive relations is transitive:"
  quickCheck prop_intrsectIsTransitive

  putStrLn "\nThe union of two transitive relations need not be transitive.\nTo preserve transitivity, one must take the transitive closure.\nThis occurs, for example, when taking the union of two equivalence relations.\nTo obtain a new equivalence relation take the transitive closure"
  quickCheck prop_unionIsNotTransitive

  putStrLn "\nTransitive Closure should be sorted"
  quickCheck prop_trClosIsSorted

  putStrLn "\nAll relations should be symmetric"
  quickCheck prop_relationsAreSymmetric

  putStrLn "\nSymmetric closure should be sorted"
  quickCheck prop_symClosIsSorted
