-- Exercise 3
module FinalAssignment.W3.Exercise3 where

import Data.List (nub, sort)
import FinalAssignment.W3.Lecture3 (Form (Cnj, Dsj), arrowfree, form1, form2, form3, nnf)
import System.Random ()
import Test.QuickCheck ()

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

flat :: Form -> Form
flat (Cnj fs) = Cnj (flatC fs)
flat (Dsj fs) = Dsj (flatD fs)
flat f = f

flatC :: [Form] -> [Form]
flatC [] = []
flatC (Cnj fs:gs) = flatC (fs++gs)
flatC (f:fs) = flat f : flatC fs

flatD :: [Form] -> [Form]
flatD [] = []
flatD (Dsj fs:gs) = flatD (fs++gs)
flatD (f:fs) = f : flatD fs   

-- Convert propositional forms to CNF
cnf :: Form -> Form
cnf = flat . cnf' . nnf . arrowfree

exercise3 :: IO ()
exercise3 = do
  putStrLn "\bExercise 3\nTime spent +/- 50 minutes\n"
  putStrLn "Form 1:"
  print $ cnf form1
  putStrLn "Form 2:"
  print $ cnf form2
  putStrLn "Form 3:"
  print $ cnf form3
