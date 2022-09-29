-- SSVT Haskell Lab
-- Week 4 - Group 5
-- Exercise 5: Use your after function to implement the functions out and ioco (infix) corresponding with 
-- the definitions in the Tretmans paper. Create tests to test the validity of your implementations.
-- Deliverables: Haskell program, tests, short test report, indication of time spent.
-- Time spend: 1,5 hours --
module Donovan.Week4.Exercise5 where
import Data.List
import Test.QuickCheck
import Donovan.Week4.LTS
import Donovan.Week4.Exercise4

-- out :: IOLTS -> [State] -> [Label]
-- out _ [] = []
-- out (states, labelI, labelU, transitions, startState) state = [label | (firsState, label, _) <- transitions,
--                                                                         firsState == head state]

out' :: IOLTS -> [State] -> [Label]
out' _ [] = []
out' (states, labelI, labelU, transitions, startState) (x:xs) = [label | (firsState, label, _) <- transitions,
                                                                        firsState == x ]

out'' :: IOLTS -> [State] -> [Label]
out'' _ [] = [" "]
out'' (states, lin, lout, ts, ss) (x:xs) = [label | (first, label, _) <- ts,
                                                                        first == x ] ++ out'' (states, lin, lout, ts, ss) xs

searchTrans :: State -> [LabeledTransition] -> [Label]
searchTrans s [] = []
searchTrans s ((s1,l,s2):ts) | s == s1  = if ts /= [] then l : searchTrans s ts else [l]
                             | otherwise = searchTrans s ts

out :: IOLTS -> [State] -> [Label]
out _ [] = []
out (states, lin, lout, ts, ss) (x:xs) = searchTrans x ts ++ out (states, lin, lout, ts, ss) xs

-- infix `ioco`
-- ioco :: IOLTS -> IOLTS -> Bool
-- ioco impl model =
--     subList (out impl (impl `after` out impl (returnFirstState impl)))
--             (out model (model `after` out model (returnFirstState model)))

infix `ioco`
ioco :: IOLTS -> IOLTS -> Bool
ioco impl model | length x > length y = False
                | otherwise = subList x y
                where
                    x = out impl (impl `after` out impl (returnFirstState impl))
                    y = out model (model `after` out model (returnFirstState model))



returnFirstState :: IOLTS -> [State]
returnFirstState (_, _, _, _, startState) = [startState]

-- This subset implementation is from https://stackoverflow.com/questions/47232335/check-if-list-is-a-sublist-of-another-list
subList :: Eq a => [a] -> [a] -> Bool
subList [] [] = True
subList _ []    = False
subList [] _    = True
subList (x:xs) (y:ys)
    | x == y    = subList xs ys
    | otherwise = subList (x:xs) ys

testIOCOs :: [(IOLTS, IOLTS, Bool)] -> IO()
testIOCOs xs = putStrLn $ testIOCOs' "" xs

testIOCOs' :: String -> [(IOLTS, IOLTS, Bool)] -> String
testIOCOs' s [] = s ++ "Finished (if no prompts showed up, your solution is correct!)"
testIOCOs' s ((impl, model, expectedResult):xs) | impl `ioco` model == expectedResult = testIOCOs' s xs
                                                | otherwise = testIOCOs' (s ++ show impl ++ " was expected " ++ (if expectedResult then "" else "not") ++ " to be IOCO with " ++ show model ++ ", but it was" ++ (if expectedResult then " not" else "") ++ " :(\n") xs

exercise5 :: IO ()
exercise5 = do
  putStrLn "\bExercise 1\nTime spent +/- - hours\n"
  putStrLn "Testing same functions for the out function as in the slides: "
  putStrLn "coffeeImpl1 for after on coin : "
  print (out coffeeImpl1 (coffeeImpl1 `after` ["coin"]))
  putStrLn "coffeeModel1 for after on coin : "
  print (out coffeeModel1 (coffeeModel1 `after` ["coin"]))
  putStrLn "coffeeImpl1 ioco coffeeModel1: "
  print (coffeeImpl1 `ioco` coffeeModel1)
  putStrLn "\n"
  testIOCOs [(coffeeImpl1, coffeeModel1, True), (coffeeImpl2, coffeeModel2, False), (coffeeImpl3, coffeeModel3, True), (coffeeImpl4, coffeeModel4, False), (coffeeImpl5, coffeeModel5, False), (coffeeImpl6, coffeeModel6, True)]
  testIOCOs [(tretmanI1, tretmanS1, True), (tretmanI2, tretmanS1, False), (tretmanI3, tretmanS1, True), (tretmanI4, tretmanS1, False),
             (tretmanI1, tretmanS2, True), (tretmanI2, tretmanS2, True), (tretmanI3, tretmanS2, True), (tretmanI4, tretmanS2, False),
             (tretmanI1, tretmanS3, False), (tretmanI2, tretmanS3, False), (tretmanI3, tretmanS3, True), (tretmanI4, tretmanS3, False),
             (tretmanI1, tretmanS4, True), (tretmanI2, tretmanS4, False), (tretmanI3, tretmanS4, True), (tretmanI4, tretmanS4, True)]