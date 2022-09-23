module FinalAssignment.W3.Exercise4 where
import Data.List ()
import System.Random ()
import Test.QuickCheck
import Test.QuickCheck.Gen
import Exercise3
import Lecture3
import Control.Monad.IO.Class


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


instance Arbitrary Form where
       arbitrary = sized genForm where
              genForm :: Int -> Gen Form
              genForm n = 
                     if n > 0 then frequency [
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
                                   Prop <$> genName
                                   ]

-- Other option: Replace "genProp =" above for "arbitrary =" and use the following function:
-- genProp :: IO Form
-- genProp = generate arbitrary
-- This created issues while attempting QuickCheck.



-- 2. Use your random testing method to test the correctness of the conversion program from Exercise4
-- Formulate a number of relevant properties to test:

-- Pre-condition: 
-- None, the cnf function should work on any propositional formula (type Form)

-- Post-conditions:
-- 1> *Output Form is logically equivalent to input Form --> Using satisfiability instead!
-- 2> Form contains no Impl
-- 3> Form contains no Equiv
-- 4> Form has Neg only on Prop Name
-- 5> Form has no redundant brackets (i.e., nested Dsj in Dsj or nested Cnj in Cnj)

-- Property 1: Satisfiability is preserved such that input and output Forms are equisatisfiable, meaning
-- that the first formula is satisfiable whenever the second is and vice versa.
propEqSatisfiable :: Form -> Property
propEqSatisfiable f = satisfiable f ==> satisfiable (cnf f)

-- Property 2: 
isImplEquiv :: [Form] -> Bool
isImplEquiv [] = True
isImplEquiv ((Impl x y):fs) = False
isImplEquiv ((Equiv x y):fs) = False
isImplEquiv (f:fs) = isImplEquiv fs

propNoImplEquiv :: Form -> Bool
propNoImplEquiv (Prop _) = True
propNoImplEquiv (Neg x) = propNoImplEquiv x
propNoImplEquiv (Cnj xs) = isImplEquiv xs
propNoImplEquiv (Dsj xs) = isImplEquiv xs
propNoImplEquiv (Impl x y) = False
propNoImplEquiv (Equiv x y) = False

-- Property 3: 
isCnj :: [Form] -> Bool
isCnj [] = True
isCnj ((Cnj _):fs) = False
isCnj (f:fs) = isCnj fs

isDsj :: [Form] -> Bool
isDsj [] = True
isDsj ((Dsj _):fs) = False
isDsj (f:fs) = isDsj fs

propNoNest :: Form -> Bool
propNoNest (Prop _) = True
propNoNest (Neg x) = propNoNest x
propNoNest (Cnj xs) = isCnj xs
propNoNest (Dsj xs) = isDsj xs
propNoNest (Impl x y) = propNoNest x && propNoNest y
propNoNest (Equiv x y) = propNoNest x && propNoNest y

-- Property 4: 


-- QuickCheck
-- Implementing QuickCheck to test our theorem.
exercise4 :: IO ()
exercise4 = do
  quickCheck (forAll propNoNest cnf genProp)