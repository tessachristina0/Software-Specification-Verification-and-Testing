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

cnf2cls :: Form -> Clauses
cnf2cls = undefined

propToInt :: Form -> Int
propToInt (Prop f) = f
propToInt (Neg (Prop f)) = -f

-- Mappen over xs
-- En dan alle properties uit de disjunction halen
-- Converteren naar int

formToClauses :: Form -> Clauses
formToClauses (Cnj xs) = map formToClause xs
formToClauses xs = formToClauses (Cnj [xs])

formToClause :: Form -> Clause
formToClause (Dsj xs) = map propToInt xs
formToClause xs = formToClause (Dsj [xs])

form = "*(1+(2*(3 4)))"

exercise6 :: IO ()
exercise6 = do
  putStrLn "\bExercise 6\nTime spent +/- ~ minutes\n"
  
  print $ parse form
