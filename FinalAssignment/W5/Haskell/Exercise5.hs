-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 1: Implement function(s) that calculate the conjectures: properties that are equivent, whose cases are subsets of other properties, etc.
-- Deliverables: implementations, documentation of approach, indication of time spent.
-- Time spend: ~ hours --
module FinalAssignment.W5.Haskell.Exercise5 where
import FinalAssignment.W5.Haskell.Mutation
import FinalAssignment.W5.Haskell.MultiplicationTable
import FinalAssignment.W5.Haskell.Exercise3
import FinalAssignment.W5.Haskell.Exercise2
import Test.QuickCheck
import Data.List

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

{- Collection of functions to be used from exercise 3: 
-}

-- Takes a mutator, the fut and an input, returns [Integer]
createMutant :: Fut -> Mutator -> Integer -> Gen [Integer]
createMutant fut mut x = mut $ fut x

-- Takes a mutator, the fut and an input and creates a list of mutants
moreMutants :: Fut -> Mutator -> Integer -> Integer -> Mutants
moreMutants fut mut x n = sequence $ [createMutant fut mut x | _ <- [1..n]]
-- One Gen [[Integer]] for each mutator...

-- All mutations!!!! 200 mutations (integer lists) per mutator
allMutants :: [Mutator] -> Fut -> Mutants
allMutants mutators fut = sequence $ concat $ [ sequence $ moreMutants fut mut 3 200 | mut <- mutators ]

-- Names of properties to print
nameProps :: [String]
nameProps = ["prop_tenElements", "prop_firstElementIsInput", "prop_sumIsTriangleNumberTimesInput", "prop_linear", "prop_moduloIsZero"]

-- Subsets: All subsets to be tested
subSets :: [[(String, Prop)]]
subSets = removeNull $ subsequences fiveProps

removeNull :: [[a]] -> [[a]]
removeNull [] = []
removeNull (x:xs) | null x     = xs
                  | otherwise   = x : removeNull xs

-- Helper: Take one subset, only the props of it to use in testers!
takeProps :: [(String, Prop)] -> [Prop]
takeProps sets = [ prop | (_, prop) <- sets]

-- Take set of props, and check if Prop holds on input with Mutant-output - one list for each mutant (so lenght sublist is nr of Props in propset)
testMutants :: [(String, Prop)] -> Mutants -> Integer -> [[Bool]]
testMutants set mutants input = [[ prop mutant input | prop <- takeProps set ] | mutant <- mutants ]
-- TODO: This &()@*)@&^ mutants is of type Gen [Integer] and needs to be just [Integer] for the above function to work. Lord knows how...
-- Attempt (not working):
-- testMutants set mutants input = do
--   mutant <- mutants
--   let bools = [[ prop mut input | prop <- takeProps set ] | mut <- mutant ]
--   return bools

-- Mutant has survived if all properties in a propset hold for the mutant 
survivor :: [Bool] -> Bool
survivor = all (== True)

-- Check for this set if, of all generated mutants, a mutant survived 
survived :: [[Bool]] -> [Bool]
survived resultsPerSet = [ survivor result | result <- resultsPerSet ]
-- Note: Result is list of True/False *per* mutant (not per prop)

-- Compares two total survivors lists and returns True if they are equivalent
compareBools :: [Bool] -> [Bool] -> Bool
compareBools fullset subset = fullset == subset


exercise5 :: IO ()
exercise5 = do
  putStrLn "\bExercise 1\nTime spent +/- ~ hours\n"
  putStrLn "\n"
