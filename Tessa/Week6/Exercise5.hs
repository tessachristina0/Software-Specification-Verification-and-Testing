-- SSVT Haskell Lab
-- Week 6 - Group 5
-- Exercise 5: Define a function that gives the transitive closure of a relation, represented as an ordered list of pairs.
-- Deliverables: Haskell program, indication of time spent.
-- Time spend: ? minutes --
module Exercise5 where
import SetOrd
import Data.List
import System.Random
import Test.QuickCheck
import Control.Monad
import Exercise4


{-
Use the datatype for relations from the previous exercise, plus the function below. 
Define a function that gives the transitive closure of a relation, represented as an ordered list of pairs. 
E.g., trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
-}

infixr 5 @@ -- infirxr is ...????

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]
-- Note: nub removes duplicates

nextRel :: Eq a => Rel a -> Rel a -> Rel a
nextRel r s = [(w, b) |
   (x, y) <- r, (w, z) <- s, y == w, (_, b) <- [(w, z)] @@ s]


trClos' :: Ord a => Rel a -> Rel a
trClos' [] = []
trClos' ((x,y):rs) = ([(x,y)] @@ rs) ++ trClos' (nextRel [(x,y)] rs) ++ trClos' rs
-- Apply to original + the new transitions, over and over again. Until list doesn't change anymore!!!
-- Look on slides for examples. 

-- Transitive closure of a relation is the original set (x,y) PLUS the relations that suffice the property of:
-- y being the x of another relation, (y,z), thus adding the relation (x,z) to the transitive closure. 
trClos ::  Ord a => Rel a -> Rel a
trClos [] = []
trClos rs = rs ++ trClos' rs

relations :: [(Integer, Integer)]
relations = [(1,2),(2,3),(3,4)]