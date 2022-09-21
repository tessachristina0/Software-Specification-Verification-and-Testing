module Exercise3 where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Control.Arrow (Arrow(arr))


-- arrowfree :: Form -> Form
-- arrowfree (Prop x) = Prop x
-- arrowfree (Neg f) = Neg (arrowfree f)
-- arrowfree (Cnj fs) = Cnj (map arrowfree fs)
-- arrowfree (Dsj fs) = Dsj (map arrowfree fs)
-- arrowfree (Impl f1 f2) =
--   Dsj [Neg (arrowfree f1), arrowfree f2]
-- arrowfree (Equiv f1 f2) =
--   Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
--   where f1' = arrowfree f1
--         f2' = arrowfree f2

-- nnf :: Form -> Form
-- nnf (Prop x) = Prop x
-- nnf (Neg (Prop x)) = Neg (Prop x)
-- nnf (Neg (Neg f)) = nnf f
-- nnf (Cnj fs) = Cnj (map nnf fs)
-- nnf (Dsj fs) = Dsj (map nnf fs)
-- nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
-- nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)


-- Convert to conjunctions of clauses with disjunctions inside:
cnf' :: Form -> Form
cnf' (Cnj fs) = Cnj (map cnf' fs)
cnf' (Dsj [x, y]) = clause (cnf' x) (cnf' y)
cnf' form = form

-- Create structure with only clauses (inner) using disjunctions, and outer conjunctions:
-- If disjunction in cnf', make the disjunction is the innermost clause - otherwise, go on
-- until you find the innermost while making the outer binds conjunctions of clauses.
clause :: Form -> Form -> Form
clause (Cnj [xs, ys]) y = Cnj [clause xs y, clause ys y]
clause y (Cnj [xs, ys]) = Cnj [clause y xs, clause y ys]
clause x y = Dsj [x, y]


-- Convert propositional forms to CNF
cnf :: Form -> Form
cnf = cnf' . nnf . arrowfree

-- Test on forms
testCnf :: Form -> Form
testCnf = cnf