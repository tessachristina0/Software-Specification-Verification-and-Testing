-- SSVT Haskell Lab
-- Week 6 - Group 5
-- Exercise 5: Suppose we implement binary relations as list of pairs, Haskell type [(a,a)]. Assume the following definition:
-- to define a function that gives the symmetric closure of a relation, where the relation is represented as an ordered list of pairs. E.g., symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].
-- Deliverables: Haskell program, indication of time spent.
-- Time spend: 3 hours --
module FinalAssignment.W6.Exercise5 where
import FinalAssignment.W6.SetOrd
import Data.List
import System.Random
import Test.QuickCheck
import Control.Monad

type Rel a = [(a,a)]

-- Function given by the UvA
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Function to give the symmetric closure of a relation. This is done using the given helper function of the UvA.
trClos ::  Ord a => Rel a -> Rel a
trClos relations = if relations == closures then sort relations else trClos closures
        where closures = relations `union` (relations @@ relations)

exercise5 :: IO ()
exercise5 = do
    putStrLn "\bExercise 1\nTime spent +/- 3 hours\n"
    putStrLn "symClos example: "
