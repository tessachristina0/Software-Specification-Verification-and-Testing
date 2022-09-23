{-# LANGUAGE BlockArguments #-}
module Exercise4 where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Test.QuickCheck (Arbitrary (arbitrary))


-- genMax :: Gen Int
-- genMax = (arbitrary :: Gen Int) `suchThat` (<= 9)

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

genProp :: Gen Form
genProp = do Prop <$> genName
    -- (3, do f <- genName
    --    return (Prop f)),

genForm :: Gen Form
genForm = [
    return genProp,
    do Neg <$> genForm,
    do f1 <- genForm
       f2 <- genForm
       return (Cnj [f1, f2]),
    do f3 <- genForm
       f4 <- genForm
       return (Dsj [f3, f4]),
    do f5 <- genForm
       Impl f5 <$> genForm,
    do f7 <- genForm
       Equiv f7 <$> genForm
    ]


genForm :: Gen Form
genForm = [
    do n <- genName
    return (Prop n),
    do f <- genForm
       return (Neg f),
    do f1 <- genForm
       f2 <- genForm
       return (Cnj [f1 f2]),
    do  
]



-- genForm :: Int -> Gen Form
-- genForm n = if n > 0 then frequency [
--     (3, do Prop <$> genName),
--     (3, do Neg <$> genForm (n - 1)),
--     (1, do f1 <- genForm (n - 1)
--            f2 <- genForm (n - 1)
--            return (Cnj [f1, f2])),
--     (1, do f3 <- genForm (n - 1)
--            f4 <- genForm (n - 1)
--            return (Dsj [f3, f4])),
--     (1, do f5 <- genForm (n - 1)
--            Impl f5 <$> genForm (n - 1)),
--     (1, do f7 <- genForm (n - 1)
--            Equiv f7 <$> genForm (n - 1))
--     ] else oneof [
--        Prop <$> genName
--     ]

-- Latest Exercise4:
instance Arbitrary Form where
       arbitrary = sized genForm

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


-- _________________

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
-}

-- Generator to create a Prop Name id
genName :: Gen Int
genName = do arbitrary `suchThat` (<=10)

-- Generator to create an arbitrary propositional formula
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
1> *Output Form is logically equivalent to input Form 
Instead we used  Using satisfiability instead!
2> Form contains no Impl
3> Form contains no Equiv
4> Form has Neg only on Prop Name
5> Form has no redundant brackets (i.e., nested Dsj in Dsj or nested Cnj in Cnj)
-}

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
  quickCheck (forAll propNoNest cnf genForm)


-- 


-- Generator to create an arbitrary propositional formula
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
1> *Output Form is logically equivalent to input Form 
Instead we used  Using satisfiability instead!
2> Form contains no Impl
3> Form contains no Equiv
4> Form has Neg only on Prop Name
5> Form has no redundant brackets (i.e., nested Dsj in Dsj or nested Cnj in Cnj)
-}

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
  quickCheck (forAll propNoNest cnf genForm)