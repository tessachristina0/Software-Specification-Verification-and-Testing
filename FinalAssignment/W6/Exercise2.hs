-- SSVT Haskell Lab
-- Week 6 - Group 5
-- Exercise 1: 
-- Time spend: ~ hours --
module FinalAssignment.W6.Excercise2 where
import FinalAssignment.W6.SetOrd
import Test.QuickCheck
import Data.List
import FinalAssignment.W6.Exercise1
import Test.QuickCheck (Arbitrary)

-- instance Arbitrary (Set a) where
--   arbitrary :: Gen (Set a)
--   arbitrary = oneof (map (return (Set a)))

-- Implementations
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set []) set2 = Set []
setIntersection set1 (Set []) = Set []
setIntersection (Set set1) (Set set2) = Set (set1 `intersect` set2)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set []) set2 = Set []
setDifference set1 (Set []) = Set []
setDifference (Set set1) (Set set2) =  Set(set1 \\ set2)

-- Testing using custom generator
prop_1 :: IO Bool
prop_1 = do 
    (Set set1) <- randomSetQuickCheck 10 0 100
    (Set set2) <- randomSetQuickCheck 10 0 100
    return (setIntersection (Set set1) (Set set2) == Set (set1 `intersect` set2))

prop_1' :: IO Bool
prop_1' = do 
    (Set set1) <- randomSet 10 0 100
    (Set set2) <- randomSet 10 0 100
    return (setIntersection (Set set1) (Set set2) == Set (set1 `intersect` set2))

prop_2 :: IO Bool
prop_2 = do 
    (Set set1) <- randomSetQuickCheck 10 0 100
    (Set set2) <- randomSetQuickCheck 10 0 100
    return (setUnion (Set set1) (Set set2) == unionSet (Set set1) (Set set2))

prop_2' :: IO Bool
prop_2' = do 
    (Set set1) <- randomSet 10 0 100
    (Set set2) <- randomSet 10 0 100
    return (setUnion (Set set1) (Set set2) == unionSet (Set set1) (Set set2))

prop_3 :: IO Bool
prop_3 = do 
    (Set set1) <- randomSetQuickCheck 10 0 100
    (Set set2) <- randomSetQuickCheck 10 0 100
    return (setDifference (Set set1) (Set set2) == Set (set1 \\ set2))

prop_3' :: IO Bool
prop_3' = do 
    (Set set1) <- randomSet 10 0 100
    (Set set2) <- randomSet 10 0 100
    return (setDifference (Set set1) (Set set2) == Set (set1 \\ set2))

-- Testing using QuickCheck
prop_1_qc :: Ord a => Set a -> Set a -> Bool
prop_1_qc (Set set1) (Set set2) = setIntersection (Set set1) (Set set2) == Set (set1 `intersect` set2)

prop_2_qc :: Ord a => Set a -> Set a -> Bool
prop_2_qc (Set set1) (Set set2) = setUnion (Set set1) (Set set2) == unionSet (Set set1) (Set set2)

prop_3_qc :: Ord a => Set a -> Set a -> Bool
prop_3_qc (Set set1) (Set set2) = setDifference (Set set1) (Set set2) == Set (set1 \\ set2)

exercise2 :: IO ()
exercise2 = do
    putStrLn "\bExercise 2\nTime spent +/- 1 hour\n"
    putStrLn "setIntersection test without quickCheck"
    randomSet 20 0 100

    putStrLn "setIntersection test with quickCheck"
    quickCheck prop_1_qc
    putStrLn "Random Set using QuickCheck: "
    randomSetQuickCheck 20 0 100
    putStrLn ""