-- SSVT Haskell Lab 
-- Week 1 - Group 5
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

-- Function to define accusations from Matthew to other persons.
accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True

-- Function to define accusations from Peter to other persons.
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False

-- Function to define accusations from Jack to other persons.
accuses Jack b = not (accuses Matthew b || accuses Peter b)
accuses Arnold b = accuses Matthew b `xor` accuses Peter b
accuses Carl b = not $ accuses Arnold b

-- Function to list all of the accusers by checking if the accuser accues. After that put that person in the list.
accusers :: Boy -> [Boy]
accusers accused = [accuser | accuser <- boys, accuses accuser accused]

-- Obtain the list of the guilty boy by checking the amount of accusers that a boy 'b' has compared to the amount of honest boys.
-- If a boy is accused by three boys then that makes him the guilty boy. Because three boys always speak the truth.
guilty, honest :: [Boy]
guilty = [b | b <- boys, length (accusers b) == amountOfHonestBoys]
honest = concat [accusers g | g <- guilty]

-- The singleton list with guilty boys is printed as well as the list with three honest boys.
main :: IO ()
main = do
    putStrLn $ "Guilty: " ++ show guilty
    putStrLn $ "Honest: " ++ show honest

-- {Time spent}: 70 minutes