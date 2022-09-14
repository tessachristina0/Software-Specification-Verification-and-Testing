-- Tim van Ekert
-- Exercise 1
module Exercise1 where
import System.Random ( Random(random), getStdRandom )

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n-1)
  return (p:ps)

calculateQuartileNumber :: [Float] -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
calculateQuartileNumber [_] a = a
calculateQuartileNumber (x:xs) (a,b,c,d) | x <= 0.25 = calculateQuartileNumber xs (a+1,b,c,d)
                                         | x <= 0.50 = calculateQuartileNumber xs (a,b+1,c,d)
                                         | x <= 0.75 = calculateQuartileNumber xs (a,b,c+1,d)
                                         | x <= 1    = calculateQuartileNumber xs (a,b,c,d+1)

checkPercentagePerQuartile :: (Int, Int, Int, Int) -> ((Int, Bool), (Int, Bool), (Int, Bool), (Int, Bool))
checkPercentagePerQuartile (a, b, c, d) = ((a, checkDiff a), (b, checkDiff b), (c, checkDiff c), (d, checkDiff d))

-- 5% margin which makes the numbers 2375 and 2625 with 2500 as checkup number
checkDiff :: Int -> Bool
checkDiff x = x >= 2375 && x <= 2625

exercise1 :: IO ()
exercise1 = do
  xs <- probs 10000
  print (checkPercentagePerQuartile(calculateQuartileNumber xs (0,0,0,0)))