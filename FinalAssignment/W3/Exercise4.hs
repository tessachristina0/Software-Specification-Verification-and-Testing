-- SSVT Haskell Lab
-- Week 3 - Group 5
-- Exercise 3: Generate formulas
-- Deliverables: generator for formulas, sequence of test properties, test report, indication of time spent.
-- Time spend: 10 hours --
module Exercise4 where
import Data.List ()
import System.Random ()
import Test.QuickCheck
import Test.QuickCheck.Gen
import Exercise3
import Lecture3
import Control.Monad.IO.Class


-- 1. Write a formula generator for random testing of properties of 
-- propositional logic:

{- For this step we attempted to learn QuickCheck well enough by closely studying
examples of generators for data Tree structures. Starting off by creating a function
genForm for generating propositional formulas with genName for the Name-Int of Prop Name 
types for names not exceeding "10" regarding readability.
An instance Arbitrary is created for genForm to recursively create formulas of any length,
however these formulas became often quite large. Different attempts were made to limit this
length outsize using sized, as well as inside Cnj and Dsj patterns - which failed - as follows.

Using vector: 
genMaxForm :: Int -> Gen [Form]
genMaxForm n | n <2 = vector 2
             | otherwise = vector n
With inside genForm: return (Cnj (resize (n `div` 2) (genMaxForm n)))

Using vectorOf:
genMaxForms :: Int -> Gen [Form]
genMaxForms n = do
       a <- genForm
       k <- choose (2, n `div` 2)
       f <- vectorOf k (genMaxForms (n `div` 4))
       return (a f)

Eventually we went for an alternative using a simple arbitrary Int n which, if larger than 0, 
would count down in every recursive calling inside genForm until zero to create a quicker stop.
Also, we failed to get output on the QuickCheck. Therefore we adapted a couple of things on genForm
namely we made arbitrary to call just a Prop Name whenever called, and made sure that genForm' as the
recursive generator would call genForm in case of n = 0. This resulted in relatively shorter and more
testable formulas for use in the rest of our tests. 
-}

-- Generator to create a Prop Name ID
genName :: Gen Int
genName = do arbitrary `suchThat` (<=10)

-- Generator to create an arbitrary propositional formula
instance Arbitrary Form where
--     arbitrary :: Gen Form
    arbitrary = Prop <$> choose (1,5)

genForm :: Gen Form
genForm = sized genForm'

genForm' :: Int -> Gen Form
genForm' 0 = arbitrary
genForm' n = if n > 0 then frequency [
                            (3, do Prop <$> genName),
                            (3, do Neg <$> genForm' (n - 1)),
                            (1, do f1 <- genForm' (n - 1)
                                   f2 <- genForm' (n - 1)
                                   return (Cnj [f1, f2])),
                            (1, do f3 <- genForm' (n - 1)
                                   f4 <- genForm' (n - 1)
                                   return (Dsj [f3, f4])),
                            (1, do f5 <- genForm' (n - 1)
                                   Impl f5 <$> genForm' (n - 1)),
                            (1, do f7 <- genForm' (n - 1)
                                   Equiv f7 <$> genForm' (n - 1))
                            ] else oneof [
                                   Prop <$> genName
                                   ]

-- Other option: Use the following function as an extra as suggested by QuickCheck guides for the 
-- formula output
--     genProp :: IO Form
--     genProp = generate arbitrary
-- However, this created issues while attempting QuickCheck.


-- 2. Use your random testing method to test the correctness of the conversion program from Exercise4
-- Formulate a number of relevant properties to test:

{- Pre-condition: 
None, the cnf function should work on any propositional formula (type Form)

Post-conditions:
1> Output Form is logically equivalent to input Form 
--> Instead we used satisfiability, to avoid an exponential increase in size due to DeMorgans Law for instance. 

2> Form contains no Implications
3> Form contains no Equivalence
4> Form has Neg only on Prop Name
5> Form has no redundant brackets (i.e., nested Dsj in Dsj or nested Cnj in Cnj)
-}

-- Property 1: Satisfiability is preserved such that input and output Forms are equisatisfiable, meaning
-- that the first formula is satisfiable whenever the second is (and the other way around).
propEqSatisfiable :: Form -> Property
propEqSatisfiable f = satisfiable f ==> satisfiable (cnf f)

-- Property 2: There are now arrows, so no Impl or Equiv
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

-- Property 4: There is no double negation


-- Property 3: There are no nested conjunctions or disjunctions
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



-- QuickCheck
-- Implementing QuickCheck to test our CNF transformer from Exercise 4
exercise4 :: IO ()
exercise4 = do
       putStrLn "\bExercise 3\nTime spent +/- 10 hours\n"
       -- quickCheck (forAll propNoNest cnf genProp)

-- QuickCheck did not go as expected, as it kept giving errors on the types of the cnf function
-- and genProp. We were unable to figure out how to make it functional, but: The intention was 
-- to run QuickCheck on forAll "generated formulas to test property" property - with the formula 
-- given to test being translated by function cnf.