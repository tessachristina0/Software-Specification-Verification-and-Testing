-- SSVT Haskell Lab
-- Week 6 - Group 5
-- Exercise 1: 
-- Time spend: ~ hours --
module FinalAssignment.W6.Excercise2 where
import FinalAssignment.W6.SetOrd
import Test.QuickCheck
import Data.List


setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set []) set2 = Set []
setIntersection set1 (Set []) = Set []
setIntersection (Set s1) (Set s2) = Set (s1 `intersect` s2)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference = undefined