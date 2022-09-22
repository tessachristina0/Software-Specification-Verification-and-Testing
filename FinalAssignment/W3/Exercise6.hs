-- Tim van Ekert
-- Exercise 1
module Exercise6 where
import Data.List
import System.Random
import Test.QuickCheck
import FinalAssignment.W3.Lecture3
import FinalAssignment.W3.Exercise3

type Clause  = [Int]
type Clauses = [Clause]

propToInt :: Form -> Int
propToInt (Prop f) = f
propToInt (Neg (Prop f)) = -f

formToClause :: Form -> Clause
formToClause (Dsj xs) = map propToInt xs
formToClause xs = formToClause (Dsj [xs])

-- Mappen over xs
-- En dan alle properties uit de disjunction halen
-- Converteren naar int

cnf2cls :: Form -> Clauses
cnf2cls (Cnj xs) = map formToClause xs
cnf2cls xs = cnf2cls (Cnj [xs])

form = "*(1+(2*(3 4)))"

cls :: Form -> Clauses
cls f = cnf2cls $ cnf f

exercise6 :: IO ()
exercise6 = do
  putStrLn "\bExercise 6\nTime spent +/- ~ minutes\n"

  print $ parse form
  print $ parse form
