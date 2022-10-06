-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 3: Implement a function that calculates the minimal property subsets, given a 'function under test' and a set of properties
-- Deliverables: implementation, documentation of approach, indication of time spent.
-- Time spend: ~ hours --
module FinalAssignment.W5.Haskell.Exercise3 where
import FinalAssignment.W5.Haskell.Mutation
import FinalAssignment.W5.Haskell.MultiplicationTable
import FinalAssignment.W5.Haskell.Exercise2
import Test.QuickCheck
import Data.List

-- Conjuctie, Disjunctie en equiv

-- Stap 1: Per property ga je elke mutator af
-- Stap 2: Alle resultaten checken

-- createMatrix' _ [] _ _ = []

-- tessa :: Eq a => Int -> (a -> Integer -> Bool) -> [a -> Gen a] -> (Integer -> a) -> Gen [Maybe Bool]
-- tessa nrOfMutants prop mutators fn = aa >>= \a -> return (concat a)
--   where aa = sequence $ [vectorOf nrOfMutants (mutate mut prop fn 3) | mut <- mutators]
-- 
-- ee = generate $ mapM (\prop -> tessa 1 prop [addElements, removeElements] multiplicationTable) multiplicationTableProps
-- donovan = generate $ mutate' removeElements [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput] multiplicationTable 3


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

removeNull :: [[a]] -> [[a]]
removeNull [] = []
removeNull (x:xs) | null x     = xs
                  | otherwise   = x : removeNull xs

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
