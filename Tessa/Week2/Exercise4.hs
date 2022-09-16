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
import Data.List


-- 1) Write isPermutation that returns True if its arguments are permutations of each other:
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = (length xs == length ys) && forall xs (`elem` ys)

-- Methods:
-- A = Make list of all permutations of first list. 
-- Check if second list is in that list...

-- B = Check lenghts of both, if not return False; if same:
-- Per element of list1 search that element in list2
-- If element not in there, return False
-- Else (at end) return True.
-- We went for B as is visible above. 


-- 2) Define some testable properties for this function, and use a number of well-chosen lists to 
-- test isPermutation:

---- * Testable properties*

-- Property 1: The reversal of a list is a permutation
propReversed :: [Integer] -> Bool
propReversed xs = isPermutation xs $ reverse xs

-- Property 2: An identical list is a permutation
-- Since the function from S to S that maps every element to itself, called the identity function id, is also
-- considered a permutation, the property of the lists having a distinct order does not apply.
propId :: [Integer] -> Bool
propId xs = isPermutation xs xs

-- The following two properties still needed to be rewritten to fit the type used for comparing strentghs.
-- They have been used for manual testing, however.
-- Property 3: Lists are the exact same length
-- propLength :: [Int] -> [Int] -> Bool
-- propLength xs ys = isPermutation xs ys && (length xs == length ys)

-- Property 4: The sum of a list of numbers remains the same for permutations
-- propSum :: [Int] -> [Int] -> Bool
-- propSum xs ys = isPermutation xs ys && sum xs == sum ys


---- Lists for manual testing
permLists1 :: [[Int]]
permLists1 = [[1, 2, 3, 4, 5], [3, 1, 5, 2, 4], [5, 4, 3, 1, 2], [5, 2, 3, 1, 4]]

permLists2 :: [[Int]]
permLists2 = [[20, 77, 54, 90, 13, 38, 99], [77, 20, 54, 90, 13, 38, 99], [99, 20, 54, 90, 13, 38, 77], [99, 38, 54, 90, 20, 13, 77]]


-- { How complete are tests? }
-- Not very complete, given that merely seperate properties are then tested on a small amount of created 
-- lists. To have a more complete testing, it is better to have a way larger amount of test cases as well
-- as testing multiple properties of the function simultaneously. 


-- 3) Provide an ordered list of properties by strength using the weaker and stronger definitions:
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

-- Provided function for checking validity (truthness) of all elements of list.
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Provided function for comparing the strength of two properties.
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

setPropertyList :: String -> Integer -> Bool
setPropertyList p x = if p == "propReversed" then propReversed [x]
                      else (p == "propId") && propId [x]

sortPropList :: String -> String -> Ordering
sortPropList x y = if stronger [-10 .. 10] (setPropertyList x) (setPropertyList y) then GT
                   else LT

-- { You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure? }
-- For us that means, we do not have to code the responsibility to count the duplicates as well, since there aren't any. 
-- For permitations this makes it more easy to implement the function.

-- { Can you automate the test process? Use the techniques presented in this week's lecture. }
-- Yes, we can use quickcheck to automatically test the properties as follows:

-- 4) QuickCheck:
exercise4 :: IO ()
exercise4 = do
  quickCheck propId
  quickCheck propReversed
  print ("This is the sorted list with the strongest one first: ", sortBy sortPropList ["propReversed", "propId"])



