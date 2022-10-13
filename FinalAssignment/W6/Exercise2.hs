-- SSVT Haskell Lab
-- Week 6 - Group 5
-- Exercise 2: Implement operations for set intersection, set union and set difference, for the datatype Set defined in SetOrd.hs. Next, use automated testing to check that your implementation is correct. First use your own generator, next use QuickCheck. 
-- Deliverables: implementations, test properties, short test report, indication of time spent.
-- Time spend: ~1 hours --
module FinalAssignment.W6.Excercise2 where
import FinalAssignment.W6.SetOrd
import Test.QuickCheck
import Data.List
import FinalAssignment.W6.Exercise1

-- Implementation of setIntersection by using the Haskell intersect function
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set []) set2 = Set []
setIntersection set1 (Set []) = Set []
setIntersection (Set set1) (Set set2) = Set (set1 `intersect` set2)

-- Implementation of setUnion by using the setOrd unionSet function given in the assingment
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

-- Implementation of setDifference by subtracting the internal set lists from one another and return as a set
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set []) set2 = Set []
setDifference set1 (Set []) = Set []
setDifference (Set set1) (Set set2) =  Set(set1 \\ set2)

-- Testing setIntersection using custom generator
prop_1 :: IO Bool
prop_1 = do
    (Set set1) <- randomSet 10 0 100
    (Set set2) <- randomSet 10 0 100
    return (setIntersection (Set set1) (Set set2) == Set (set1 `intersect` set2))

-- Testing setIntersection using quickCheck generator
prop_1' :: IO Bool
prop_1' = do
    (Set set1) <- randomSetQuickCheck 10 0 100
    (Set set2) <- randomSetQuickCheck 10 0 100
    return (setIntersection (Set set1) (Set set2) == Set (set1 `intersect` set2))

-- Testing setUnion using custom generator
prop_2 :: IO Bool
prop_2 = do
    (Set set1) <- randomSet 10 0 100
    (Set set2) <- randomSet 10 0 100
    return (setUnion (Set set1) (Set set2) == unionSet (Set set1) (Set set2))

-- Testing setUnion using quickCheck generator
prop_2' :: IO Bool
prop_2' = do
    (Set set1) <- randomSetQuickCheck 10 0 100
    (Set set2) <- randomSetQuickCheck 10 0 100
    return (setUnion (Set set1) (Set set2) == unionSet (Set set1) (Set set2))

-- Testing setDifference using custom generator
prop_3 :: IO Bool
prop_3 = do
    (Set set1) <- randomSet 10 0 100
    (Set set2) <- randomSet 10 0 100
    return (setDifference (Set set1) (Set set2) == Set (set1 \\ set2))

-- Testing setDifference using quickCheck generator
prop_3' :: IO Bool
prop_3' = do
    (Set set1) <- randomSetQuickCheck 10 0 100
    (Set set2) <- randomSetQuickCheck 10 0 100
    return (setDifference (Set set1) (Set set2) == Set (set1 \\ set2))

exercise2 :: IO ()
exercise2 = do
    putStrLn "\bExercise 2\nTime spent +/- 1 hour\n"
    putStrLn "setIntersection test without quickCheck"
    firstProp <- prop_1
    print firstProp
    putStrLn "setIntersection test with quickCheck"
    firstPropQC <- prop_1'
    print firstPropQC
    putStrLn "setUnion test without quickCheck"
    secondProp <- prop_2
    print secondProp
    putStrLn "setUnion test with quickCheck"
    secondPropQC <- prop_2'
    print secondPropQC
    putStrLn "setDifference test without quickCheck"
    thirdProp <- prop_2
    print thirdProp
    putStrLn "setDifference test with quickCheck"
    thirdPropQC <- prop_3'
    print thirdPropQC