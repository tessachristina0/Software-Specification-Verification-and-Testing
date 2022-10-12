-- SSVT Haskell Lab
-- Week 6 - Group 5
-- Exercise 1: 
-- Time spend: ~ hours --
module FinalAssignment.W6.Excercise2 where
import FinalAssignment.W6.SetOrd
import Test.QuickCheck
import Data.List


setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection a b = undefined

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion a b = Set (setUnion' a b)

setUnion' :: Set a -> Set a -> [a]
setUnion' a b = [a ++ b]

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference = undefined