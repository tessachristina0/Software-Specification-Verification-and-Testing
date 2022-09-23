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



-- Use your random testing method to test the correctness of the conversion program from Exercise4
-- Formulate a number of relevant properties to test:
