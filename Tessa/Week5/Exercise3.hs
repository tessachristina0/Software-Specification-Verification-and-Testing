-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 3: Implement a function that calculates the minimal property subsets, given a 'function under test' and a set of properties
-- Deliverables: implementation, documentation of approach, indication of time spent.
-- Time spend: ~ hours --
module Exercise3 where
import Mutation
import MultiplicationTable
import Exercise1
import Test.QuickCheck
import Data.List (subsequences)

-- All types used, for overview
type NrOfMutants = Int
type Prop = [Integer] -> Integer -> Bool
type Fut = (Integer -> [Integer])
type Matrix = Gen [[Maybe Bool]]
type Mutator = ([Integer] -> Gen [Integer])
type Mutants = Gen [[Integer]]


allMutators :: [Mutator]
allMutators = [addElements, removeElements, addition, subtraction, multiplication, divide, modulo]

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


-- Fullset: Property functions zipped with names to be able to print eventually
fiveProps :: [(String, Prop)]
fiveProps = zip nameProps multiplicationTableProps

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
testMutants :: [(String, Prop)] -> Gen [[Integer]] -> Integer -> [[Bool]]
-- testMutants :: [(String, Prop)] -> [[Integer]] -> Integer -> [[Bool]]
testMutants set mutants input = [[ prop mutant input | prop <- takeProps set ] | mutant <- mutants ]
-- TODO: This &()@*)@&^ mutants is of type Gen [Integer] and needs to be just [Integer] for the above function to work. Lord knows how
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
compareShits :: [Bool] -> [Bool] -> Bool
compareShits fullset subset = fullset == subset

-- Returns the subset of which the results are equal to the fullset
minimalSet :: [(String, Prop)] -> [[(String, Prop)]] -> [[String]]
minimalSet fullset subsets = g [ subset | subset <- subsets, compareShits (f fullset) (f subset)]
  where
    -- f x = survived $ testMutants x (allMutants allMutators multiplicationTable) 3
    f x = do
      survived $ testMutants x (allMutants allMutators multiplicationTable) 3
    g y = [[ name | (name, _) <- minimalset ] | minimalset <- y ]


-- ALGORITHM
-- Create a mutant for a mutator
-- Create all n mutants (of fut MultiplicationTable) and one mutator
--          This is a list of mutants - where a mutant = [Int], the mutated table
-- Make one list for n mutants per mutator -> stick them together

-- Zip Propnames with the Propfunc
--         Make tuples such as [(Name, Prop)]
-- Create fullset = prop 1-5
-- Create all property subsets = subsequences of [prop 1-5]
--        Remove [] and [prop 1-5] from subsets

-- Helper function: take Props from tuples with unzip

-- Test each mutant on the fullset [(_, Prop)] (5 Bools)
--        Make list of [[Bools]] for all mutants for the fullset
-- Test each mutant on a property subset (1-4 Bools) --> Note: on Props, not tuple
--        Make list of [[Bools]] for all mutants for the subset 

-- Create function Survivor that checks if in a [Bools] list all is True
--        Gives True if all are True

-- Create function Survived that runs Survivor over [[Bools]] and returns [Bools]
--        We get a list of Bools per mutant in right order to compare with fullset!!!

-- Compare each mutant-test outcome: 
--        Check Survived(fullset Bools[i]) == Survived(subset Bools[i]) 
--        If at end of recursive formula all i is True:
--                  Return this subset (or add to list)
--                  Do same with next subset
-- 
-- End: Print all minimal sets


exercise3 :: IO ()
exercise3 = do
  putStrLn "\bExercise 1\nTime spent +/- ~ hours\n"
  putStrLn "\n"
