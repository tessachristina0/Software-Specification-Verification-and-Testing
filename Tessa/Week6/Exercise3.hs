-- SSVT Haskell Lab
-- Week 6 - Group 5
-- Exercise 3: Define a function that gives the symmetric closure of a relation, where the relation is represented as an ordered list of pairs.
-- Deliverables: Haskell program, indication of time spent.
-- Time spend: ? minutes --

{-
Suppose we implement binary relations as list of pairs, Haskell type [(a,a)]. 
Assume the following definition:
> type Rel a = [(a,a)]
Use the following declaration to define a function that gives the symmetric closure of a relation, 
where the relation is represented as an ordered list of pairs. 
Example: symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].
-}

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos _ = undefined
    
