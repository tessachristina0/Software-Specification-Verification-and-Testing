-- SSVT Haskell Lab
-- Week 2 - Group 5
-- Exercise 4: Write isPermutation that returns True if its arguments are permutations of each other. 
    -- Then define some testable properties for this function, and use a number of well-chosen lists to 
    -- test isPermutation. 
    -- Lastly, provide an ordered list of properties by strength
-- Deliverables: Haskell program, concise test report, indication of time spent.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module Exercise4 where
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    forAll,
    quickCheck,
    suchThat,
    (==>),
  )


-- 1) Write isPermutation that returns True if its arguments are permutations of each other:
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = (length xs == length ys) && checkElems xs ys

checkElems :: Eq a => [a] -> [a] -> Bool
checkElems (x:xs) ys = elem x ys && checkElems xs ys

-- Methods:
-- A = Make list of all permutations of first list. 
    -- Check if second list is in that list...

-- B = Check lenghts of both, if not return False; if same:
    -- Per element of list1 search that element in list2
    -- If element not in there, return False
    -- Else (at end) return True.



-- 2) Define some testable properties for this function, and use a number of well-chosen lists to 
-- test isPermutation:

---- * Testable properties*
-- Property 1: Lists are the exact same length
propLength :: [Int] -> [Int] -> Bool
propLength xs ys = isPermutation xs ys && (length xs == length ys)

-- Property 2: The reversal of a list is a permutation
propReversed :: [Int] -> [Int] -> Bool
propReversed xs ys = isPermutation xs ys && ys == reverse xs

-- Property 3: An identical list is a permutation
    -- Since the function from S to S that maps every element to itself, called the identity function id, is also
    -- considered a permutation, the property of the lists having a distinct order does not apply.
propId :: Eq a => [a] -> [a] -> Bool
propId xs ys = isPermutation xs ys && xs == ys

-- Property 4: The sum of a list of numbers remains the same for permutations
propSum :: [Int] -> [Int] -> Bool
propSum xs ys = isPermutation xs ys && sum xs == sum ys


-- { You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure? }
    -- ...


---- * Lists for manual testing *
permLists1 :: [[Int]]
permLists1 = [[1, 2, 3, 4, 5], [3, 1, 5, 2, 4], [5, 4, 3, 1, 2], [5, 2, 3, 1, 4]]

permLists2 :: [[Int]]
permLists2 = [[20, 77, 54, 90, 13, 38, 99], [77, 20, 54, 90, 13, 38, 99], [99, 20, 54, 90, 13, 38, 77], [99, 38, 54, 90, 20, 13, 77]]


-- { How complete are tests? }
    -- ...



-- 3) Provide an ordered list of properties by strength using the weaker and stronger definitions:
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

-- Provided function for checking validity (truthness) of all elements of list.
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Provided function for comparing the strength of two properties.
stronger, weaker :: [a] -> (a -> a -> Bool) -> (a -> a -> Bool) -> Bool
stronger xs ys p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- TODO: Attempt to test two properties:
test1 = stronger [1..10] propLength xs ys propReversed xs ys



-- { Can you automate the test process? Use the techniques presented in this week's lecture. }


-- 4) QuickCheck: