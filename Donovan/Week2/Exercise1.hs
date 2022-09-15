import Data.List
import Data.Char
import System.Random
import Test.QuickCheck


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

calculateIntervals :: [Float] -> [Integer]
calculateIntervals n | n > 0 = 

  