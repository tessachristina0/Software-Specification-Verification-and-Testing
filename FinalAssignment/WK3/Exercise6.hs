-- Tim van Ekert
-- Exercise 1
module Exercise6 where
import Data.List
import System.Random
import Test.QuickCheck
import FinalAssignment.WK3.Lecture3

type Clause  = [Int]
type Clauses = [Clause]

-- p -> q === !p v q

formExample = Equiv (Cnj p q) (Impl (Neg q) (Neg p))

cnf2cls :: Form -> Clauses
cnf2cls = undefined

exercise6 :: IO ()
exercise6 = do
  putStrLn "\bExercise 6\nTime spent +/- ~ minutes\n"