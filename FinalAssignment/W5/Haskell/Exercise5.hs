-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 1: Implement function(s) that calculate the conjectures: properties that are equivent, whose cases are subsets of other properties, etc.
-- Deliverables: implementations, documentation of approach, indication of time spent.
-- Time spend: ~ hours --
module FinalAssignment.W5.Haskell.Exercise5 where
import FinalAssignment.W5.Haskell.Mutation
import FinalAssignment.W5.Haskell.MultiplicationTable

-- ALGORITHM --
-- Below, I will describe the algorithm used for this exercise. However, given that
-- the testMutants function in previous exercises do not seem to work due to struggles
-- with the Gen Monad (see exercise 3), the functions for this exercise were not implemented.

{- 
Create a mutant for one mutator.
Create all n mutants (of fut MultiplicationTable) and one mutator. This is a list of mutants, where a 
mutant is an [Integer], i.e., the mutated multiplicationtable.
Mutant list: Make one list for n mutants for each mutator -> stick them together into 1 list.

Zip Propnames with the Propfunction: Make tuples such as [(Name, Prop)]. Then create all property 
subsets with subsequences of [prop 1-5], and remove [] and [prop 1-5] from subsets.

Helper function: take Props from tuples with unzip

Test EACH SUBSET of properties on each mutant from the mutant list. This will create a list of [Bools]
per subset.
The Survivor function checks if one of those [Bools] lists is a surviving mutant, then Survived creates
a list of all the mutants, including the surviving ones, of one subset (i.e., a [[Bools]] will be a [Bools]).

Now, every Survived-list of each subset is tested against EVERY other subset. This is done in two ways:

1. With an equality compare-function within a conjectures-function(1):
The conjenctures-function uses the compare-function to compare every mutant (i.e., the same index of the Survived-list)
in the two subsets of lenght ONE (so single property subsets). 
If all mutants are equally killed/not killed, the two subsets are equal. 
If equal, the two subsets are put in a list, so this is a list of lists of [(String, Prop)] (= the subset). 
At last, another function will read this [ [ [(String, Prop)] ] ] out, and print the equal subset-names - so
it will only print the String-part of the tuples by pattern-matching.

2. With a subset compare-function within a conjectures-function(2):
The conjectures-function uses this function to compare every mutant (each index) in the two subsets, of which 
one subset is of length ONE and the other subset is of length 2, 3 or 4. The comparison exists of checking 
whether a single property-subset is a subset of another property subset, i.e.: 
All the killed mutants in the single set are ALSO killed in the larger subset.
If this is the case, the two subsets are put in a list, which becomes a list of lists of [(String, Prop)] (= the subset).
At last, the same function as prior compare-method will print the subsets out. 

-}

exercise5 :: IO ()
exercise5 = do
  putStrLn "\bExercise 1\nTime spent +/- ~ hours\n"
  putStrLn "\n"
