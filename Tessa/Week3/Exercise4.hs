{-# LANGUAGE BlockArguments #-}
module Exercise2 where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Test.QuickCheck (Arbitrary (arbitrary), frequency)


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

genForm :: Gen Form
genForm = frequency [
    (3, do Prop <$> genName),
    (3, do Neg <$> genForm),
    (1, do f1 <- genForm
           f2 <- genForm
           return (Cnj [f1, f2])),
    (1, do f3 <- genForm
           f4 <- genForm
           return (Dsj [f3, f4])),
    (1, do f5 <- genForm
           Impl f5 <$> genForm),
    (1, do f7 <- genForm
           Equiv f7 <$> genForm)
    ]

--  vectorOf makes a list with max size n for inside bigger Forms
-- genProp :: Gen [Form]
-- genProp = vectorOf 




-- Use your random testing method to test the correctness of the conversion program from Exercise4
-- Formulate a number of relevant properties to test:
