-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 3: Implement a function that calculates the minimal property subsets, given a 'function under test' and a set of properties
-- Deliverables: implementation, documentation of approach, indication of time spent.
-- Time spend: ~ hours --
module FinalAssignment.W5.Haskell.Exercise3 where
import Exercise2
import Mutation
import FinalAssignment.W5.Haskell.MultiplicationTable
import Test.QuickCheck
import Data.List


-- METHOD ONE:
{- This initial method compares the exact result of each mutant tested with a subset of properties, to the result of the same
mutant tested with the full set of properties. However, there was one deficit in the implementation of this algorithm - namely
in the testMutants function. In that function there is a Gen [[Integer]] type for a list of mutant, of which merely an [[Integer]]
can be used as input within the function. After many attempts it could not be fixed. Thus, we present another second method after 
this one.
-}
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

-- All types used, for overview
type NrOfMutants = Int
type Fut = (Integer -> [Integer])
type Matrix = Gen [[Maybe Bool]]
type Mutants = Gen [[Integer]]

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
compareShits :: [Bool] -> [Bool] -> Bool
compareShits fullset subset = fullset == subset

-- Returns the subset of which the results are equal to the fullset
minimalSet :: [(String, Prop)] -> [[(String, Prop)]] -> [[String]]
minimalSet fullset subsets = g [ subset | subset <- subsets, compareShits (f fullset) (f subset)]
  where
    f x = survived $ testMutants x (allMutants allMutators multiplicationTable) 3
    g y = [[ name | (name, _) <- minimalset ] | minimalset <- y ]




-- METHOD TWO:
{- Given the lack of output of the previous function, below follows the second implementation. This implementation
compares the number of survivors of the same amount of mutants tested for a subset, opposed to the full set of properties.
Thus, this is not an exact comparison of the same mutants, but does provide a test result, however not as exact as method 1.
-}

-- GENERATES:
-- [
--   [Mutator 1 x 
--     [Mutation x Prop1, Mutation x Prop2, Mutation x Prop3]
--   ],
--   [Mutator 2 x 
--     [Mutation x Prop1, Mutation x Prop2, Mutation x Prop3] 
--   ]
-- ]

-- 1. [Prop_1, Prop_2, Prop_3, Prop_4]
-- 2. [Prop_1, Prop_2], [Prop_1, Prop_3], [Prop_2, Prop_3]
-- 3. mutate' 

ee :: Gen [[Bool]]
ee = sequence $ [mutate' mut multiplicationTableProps multiplicationTable 1 | mut <- allMutators ]

-- gg = filter (\y -> all (== True) y && not (null y)) (fmap (\props -> mutate' addElements props multiplicationTable 1) ff)

-- ([Int], [Bool])

numberedProps :: [(Int, [Integer] -> Integer -> Bool)]
numberedProps = zip [1..] multiplicationTableProps

minimalProps = do
   [([i | (i, _) <- c], ee) | c <- subsequences numberedProps]

ff = testsMatrix >>= \tests -> return tests
  where
    testsMatrix = sequence $ [([1], mutate' addElements props multiplicationTable 1) | props <- subsequences multiplicationTableProps]

--           p1    p2    p3    p4    p5
-- mutator  [False,False,False,False,True],
-- mutator  [False,True ,False,True ,True],
-- mutator  [True ,False,False,True ,True],
-- mutator  [True ,False,False,True ,True],
-- mutator  [True ,False,False,False,True],
-- mutator  [True ,False,False,False,True],
-- mutator  [True ,False,False,False,True]

-- [(1, 2, 3), [False, False, False]]

--           p1    p2    p3    p4
-- mutator  [False,False,False,False],
-- mutator  [False,True ,False,True ],
-- mutator  [True ,False,False,True ],
-- mutator  [True ,False,False,True ],
-- mutator  [True ,False,False,False],
-- mutator  [True ,False,False,False],
-- mutator  [True ,False,False,False]

--           p1    p3    p4
-- mutator  [False,False,False],
-- mutator  [False,False,True ],
-- mutator  [True ,False,True ],
-- mutator  [True ,False,True ],
-- mutator  [True ,False,False],
-- mutator  [True ,False,False],
-- mutator  [True ,False,False]

--           p1    p4
-- mutator  [False],
-- mutator  [False],
-- mutator  [],
-- mutator  [],
-- mutator  [False],
-- mutator  [False],
-- mutator  [False]

-- createMatrix' :: Int -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [Gen [[Bool]]]
-- createMatrix' nrOfMutants props fn = map (\mut -> vectorOf nrOfMutants (mutate' mut props fn 1)) allMutators

-- remove mutator array
-- [
--   [Mutation x Prop1, Mutation x Prop2, Mutation x Prop3],
--   [Mutation x Prop1, Mutation x Prop2, Mutation x Prop3],
-- ]

-- test3 = generate $ sequence $ createMatrix' 2 [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput] multiplicationTable

namedSubsets :: [(Int, Prop)]
namedSubsets = zip [1..] multiplicationTableProps

namedSubsetss :: [(Int, String)]
namedSubsetss = zip [1..] multiplicationTablePropss

-- Creates all subsets
multiplicationTablePropss = ["prop_tenElements", "prop_firstElementIsInput", "prop_sumIsTriangleNumberTimesInput", "prop_linear", "prop_moduloIsZero"]

subsets :: [a] -> [[a]]
subsets props = [ subset | subset <- subsequences props]

fullset :: [Prop] -> Gen Int
fullset props = countSurvivors 4000 props multiplicationTable

-- Creates a matrix of each subset with its nr of survivors
-- subsetsMatrix :: [[Prop]] -> [([(Int, Prop)], Gen Int)]
-- subsetsMatrix subsets = [(subset, countSurvivors 4000 subset multiplicationTable) | subset <- subsets ]


aa :: Gen [([Int], Int)]
aa = sequence [sequence ([i | (i, _) <- v], countSurvivors 4000 [a | (_, a) <- v] multiplicationTable) | v <- removeNull $ subsets namedSubsets]

vv = fmap (filter (\(props, score) -> score == 0 && length props < 5)) aa


-- bb = [[], countSurvivors 4000 [a | (i, a) <- v] multiplicationTable | v <- removeNull $ subsets namedSubsets]

-- cc = [[i | (i, _) <- v] | v <- removeNull $ subsets namedSubsetss, compareProps multiplicationTableProps [j | (_, j) <- v] multiplicationTable >>= x]

compareProps allProps props fn = do
  a1 <- mutate' addElements allProps fn 1
  a2 <- mutate' addElements props fn 1
  return (a2 == a1)

iii props allProps fn = a1 >>= \x -> a2 >>= \y -> return (x == y)
  where   a1 = mutate' addElements allProps fn 1
          a2 = mutate' addElements props fn 1

-- dd = yy >>= \x -> return (elemIndex (foldl1' min x) x)
--   where yy = sequence bb

-- -- -- Compares each the survivors of each subset to the survivors of the full set of properties
-- comparesets :: Gen Int -> [([Prop], Gen Int)] -> [[Prop]]
-- comparesets allprops [] = []
-- comparesets allprops ((prop, subset):xs) | allprops == subset  = prop : comparesets allprops xs
--                                          | otherwise           = comparesets allprops xs


exercise3 :: IO ()
exercise3 = do
  putStrLn "\bExercise 1\nTime spent +/- ~ hours\n"
  putStrLn "\n"
