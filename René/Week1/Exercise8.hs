import Data.Bits (xor)

data Boy = Matthew | Peter | Jack | Arnold | Carl
  deriving (Eq, Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

amountOfHonestBoys :: Int
amountOfHonestBoys = 3

accuses :: Boy -> Boy -> Bool
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew _ = True

accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter _ = False

accuses Jack b = not (accuses Matthew b || accuses Peter b)
accuses Arnold b = accuses Matthew b `xor` accuses Peter b
accuses Carl b = not $ accuses Arnold b

accusers :: Boy -> [Boy]
accusers accused = [accuser | accuser <- boys, accuses accuser accused]

guilty, honest :: [Boy]
guilty = [b | b <- boys, length (accusers b) == amountOfHonestBoys]
honest = concat [accusers g | g <- guilty]

main :: IO ()
main = do
    putStrLn $ "Guilty: " ++ show guilty
    putStrLn $ "Honest: " ++ show honest