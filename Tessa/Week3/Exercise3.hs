module Exercise3 
where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Control.Arrow (Arrow(arr))


arrowfree :: Form -> Form
arrowfree (Prop x) = Prop x
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) =
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) =
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2

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
cnf' (Dsj [x, y]) = deMorgan (cnf' x) (cnf' y)
cnf' form = form


-- DeMorgan
deMorgan :: Form -> Form
deMorgan (Dsj [Cnj l:ls, Cnj fs]) = Cnj [(deMorgan $ Dsj [l (Cnj fs)]) (deMorgan $ Dsj [ls (Cnj fs)])]
deMorgan (Dsj [Cnj l:ls, Dsj fs]) = Cnj [(deMorgan $ Dsj [l (Disj fs)]) (deMorgan $ Dsj [ls (Disj fs)])]
deMorgan (Dsj [Dsj ls, Cnj f:fs]) = Cnj [(deMorgan $ Dsj [deMorgan Dsj ls f]) (deMorgan $ Dsj [deMorgan Dsj ls fs])]
deMorgan (Dsj [Dsj ls Dsj fs]) = deMorgan Dsj [ls fs]
-- deMorgan (Dsj ls) = Dsj ls
deMorgan form = noNest form -- Remove parenthesis


-- Remove parenthesis
noNest :: Form -> Form -- Cnj and Dsj as f?
noNest (f [x, f y]) = noNest f [x y] where type :: f = Form
noNest (f [f x, y]) = noNest f [x y]
-- noNest (Cnj [x, Cnj y]) = noNest Cnj [x y]
-- noNest (Cnj [Cnj x, y]) = noNest Cnj [x y]
noNest form = form

-- ----------------------


-- TODO: Make stop-condition function to check clauses
-- Create structure with only clauses (inner) using disjunctions, and outer conjunctions:
-- If disjunction in cnf', make the disjunction is the innermost clause - otherwise, go on
-- until you find the innermost while making the outer binds conjunctions of clauses.
clause :: Form -> Form -> Form
-- clause (Cnj [xs, ys]) y = Cnj [clause xs y, clause ys y]
-- clause x (Cnj [xs, ys]) = Cnj [clause x xs, clause x ys]
clause (Dsj li) y = Dsj (li++y)
clause x (Dsj [xs, ys]) = Dsj (x:xs:ys) -- Clause takes 2 args -> what if y is another Disj.? 
                                        -- How to test if y consists of another conj/disj, if it potentially has only 1 arg (as Prop)?
clause (Prop x) (Prop y) = Dsj [x, y]


-- TODO: Function to remove duplicates
rem :: Form -> Form
rem (Cnj fs) = Cnj (rem' fs)
rem (Dsj fs) = Dsj (rem' fs)
rem form = form

rem' :: [Form] -> [Form]
rem' [] = []
rem' (Cnj xs:ys) = rem' (xs:ys)
rem' (Dsj xs:ys) = rem' (xs:ys)
rem' (f:fs) = rem f : rem' fs

-- rem' (xs:ys) = case xs:ys of
--   :: Cnj -> rem' (xs++ys)
--   :: Dsj -> rem' (xs++ys)



-- Convert propositional forms to CNF
cnf :: Form -> Form
cnf = cnf' . nnf . arrowfree

-- Test on forms
testCnf :: Form -> Form
testCnf = cnf