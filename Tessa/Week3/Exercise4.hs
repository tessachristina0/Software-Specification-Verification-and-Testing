module Exercise4 where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Data.Bool (Bool (True, False))
import Exercise3


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
genName = arbitrary `suchThat` (<=10)

instance Arbitrary Form where
    arbitrary = sized genForm


-- Resize to adapt the implicit generator of the arbitrary function.
genForm :: Int -> Gen Form
genForm n = frequency [
    (3, Prop <$> genName),
    (3, Neg <$> genForm n), -- resize :: int -> gen [form] -> gen [form]
    (1, return (Cnj (resize (n `div` 2) (genMaxForm n)))),
    (1, return (Dsj (resize (n `div` 2) (genMaxForm n)))),
    (1, do f5 <- genForm
           Impl f5 <$> genForm n),
    (1, do f7 <- genForm
           Equiv f7 <$> genForm n)
    ]

-- vector met ten minste n = 2
genMaxForm :: Int -> Gen [Form]
genMaxForm n | n <2 = vector 2
             | otherwise = vector n


-- genMaxForms :: Int -> Gen [Form]
-- genMaxForms n = do
--     a <- genForm
--     k <- choose (2, n `div` 2)
--     f <- vectorOf k (genMaxForms (n `div` 4))
--     return (a f)


-- Use your random testing method to test the correctness of the conversion program from Exercise4
-- Formulate a number of relevant properties to test:

-- * Properties for testing CNF *
-- PRE: 
-- None, the cnf function should work on any propositional formula (type Form)

-- POST:
-- *Output Form is logically equivalent to input Form
-- Form contains no Impl
-- Form contains no Equiv
-- Form has Neg only on Prop Name
-- Form has no redundant brackets (i.e., nested Dsj in Dsj or nested Cnj in Cnj)

-- Satisfiability is preserved such that input and output Forms are equisatisfiable, meaning 
-- that the first formula is satisfiable whenever the second is and vice versa.
propEqSatisfiable :: Form -> Bool
propEqSatisfiable f = satisfiable f ==> satisfiable (cnf f) && satisfiable (cnf f) ==> satisfiable f

propNoImplEquiv :: Form -> Bool
propNoImplEquiv f | Prop _ = True
                  | Neg x = propNoImpl x
                  | Cnj x:xs = propNoImpl x && propNoImpl xs
                  | Dsj x:xs = propNoImpl x && propNoImpl xs
                  | Impl x y = False
                  | Equiv x y = False

isCnj :: [Form] -> Bool
isCnj (f:fs) = case f of
    [] -> True
    Cnj xs -> False
    _ -> isCnj fs

isDsj :: [Form] -> Bool
isDsj (f:fs) = case f of
    [] -> True
    Dsj xs -> False
    _ -> isDsj fs

propNoNest :: Form -> Bool
propNoNest f | Prop _ = True
             | Neg x = propNoNest x
             | Cnj xs = isCnj xs
             | Dsj xs = isDsj xs