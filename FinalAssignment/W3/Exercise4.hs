{-# LANGUAGE BlockArguments #-}
module FinalAssignment.W3.Exercise4 where
import Data.List ()
import System.Random ()
import Test.QuickCheck
import Test.QuickCheck.Gen
import FinalAssignment.W3.Lecture3 ( Form(..) )


-- type Name = Int

-- data Form = Prop Name
--           | Neg  Form
--           | Cnj [Form]
--           | Dsj [Form]
--           | Impl Form Form
--           | Equiv Form Form
--           deriving (Eq,Ord)


-- Write a formula generator for random testing of 
-- properties of propositional logic:
genName :: Gen Int
genName = do arbitrary `suchThat` (<=10)

genForm :: Int -> Gen Form
genForm n = if n > 0 then frequency [
    (3, do Prop <$> genName),
    (3, do Neg <$> genForm (n - 1)),
    (1, do f1 <- genForm (n - 1)
           f2 <- genForm (n - 1)
           return (Cnj [f1, f2])),
    (1, do f3 <- genForm (n - 1)
           f4 <- genForm (n - 1)
           return (Dsj [f3, f4])),
    (1, do f5 <- genForm (n - 1)
           Impl f5 <$> genForm (n - 1)),
    (1, do f7 <- genForm (n - 1)
           Equiv f7 <$> genForm (n - 1))
    ] else oneof [
       (Prop <$> genName)
    ]

--  vectorOf makes a list with max size n for inside bigger Forms
-- genProp :: Gen [Form]
-- genProp = vectorOf 




-- Use your random testing method to test the correctness of the conversion program from Exercise4
-- Formulate a number of relevant properties to test:
