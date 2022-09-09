-- SSVT Haskell Lab 
-- Week 1 - Group C
-- Exercise 8: Solve a crime by one guilty boy in a school of boys, of which 3 are honest.
-- Deliverables: Haskell program, indication of time spent.

import Data.Bits (xor)

data Boy = Matthew | Peter | Jack | Arnold | Carl
  deriving (Eq, Show)

-- Define the boys
boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Define the number of honest boys.
amountOfHonestBoys :: Int
amountOfHonestBoys = 3

-- Function to define ...
accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True

-- Function to define ...
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False

-- Function to...
accuses Jack b = not (accuses Matthew b || accuses Peter b)
accuses Arnold b = accuses Matthew b `xor` accuses Peter b
accuses Carl b = not $ accuses Arnold b

-- Function to ...
accusers :: Boy -> [Boy]
accusers accused = [accuser | accuser <- boys, accuses accuser accused]

-- Obtain the list of quilty boys by ... etc.
guilty, honest :: [Boy]
guilty = [b | b <- boys, length (accusers b) == amountOfHonestBoys]
honest = concat [accusers g | g <- guilty]

-- TODO: 
-- Our solution: The guilty boy is given by... *explanation*
main :: IO ()
main = do
    putStrLn $ "Guilty: " ++ show guilty
    putStrLn $ "Honest: " ++ show honest

-- {Time spent}: 70 minutes