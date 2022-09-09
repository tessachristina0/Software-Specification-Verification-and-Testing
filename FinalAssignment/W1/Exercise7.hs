-- SSVT Haskell Lab 
-- Week 1 - Group C
-- Exercise 7: 
-- Deliverables: Haskell program, test of correctness, indication of time spent.

-- Provided function for checking validity (truthness) of all elements of list.
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- TODO:
toDigitArray :: Integer -> [Integer]
toDigitArray 0 = []
toDigitArray x = toDigitArray (x `div` 10) ++ [x `mod` 10]

-- TODO: 
digitArrayToInteger :: [Integer] -> Integer
digitArrayToInteger = read . concatMap show

-- TODO: 
addDigitsOfProduct :: Integer -> Integer
addDigitsOfProduct n
  | n > 9 = sum $ toDigitArray n
  | otherwise = n

-- TODO: 
doubleEverySecondDigit :: [Integer] -> [Integer]
doubleEverySecondDigit [] = []
doubleEverySecondDigit l = [if even i then addDigitsOfProduct (v * 2) else v | (i, v) <- zip [1 ..] l]

-- Function to test the Luhn property of a creditcard number.
luhn :: Integer -> Bool
luhn n = sum (doubleEverySecondDigit $ reverse $ toDigitArray n) `mod` 10 == 0

-- Distinct types of creditcards defined by their characteristics, used for testing correctness.
isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n =
  x == 3 && (y == 4 || y == 7)
    && length (x : y : xs) == 15
    && luhn n
  where
    (x : y : xs) = toDigitArray n
isVisa n =
  x == 4
    && (length (x : xs) == 13 || length (x : xs) == 16)
    && luhn n
  where
    (x : xs) = toDigitArray n
isMaster n =
  (x == 5 && (y >= 1 || y <= 5))
    || (digitArrayToInteger (take 4 (x : y : xs)) >= 2221 || digitArrayToInteger (take 4 (x : y : xs)) <= 2720)
    && length (x : y : xs) == 16
    && luhn n
  where
    (x : y : xs) = toDigitArray n

-- Test for correctness: Ror each creditcard type using lists of valid creditcard numbers.
testIsAmericanExpress, testIsMaster, testIsVisa :: [Integer] -> Bool
testIsAmericanExpress n = forall n isAmericanExpress
testIsMaster n = forall n isMaster
testIsVisa n = forall n isVisa

americanExpressNumbers :: [Integer]
americanExpressNumbers =
  [ 347836685275538,
    378314665412507,
    374932653895476,
    378248786355155,
    342177129670314,
    342224214963087,
    379590292193318,
    378690436716643,
    379907512930743,
    374341965694272,
    378440664616069,
    341742513629852,
    378235598227268,
    347887471340895,
    346691045520177,
    377803751449002,
    341136200308518,
    341413098673588,
    348764327754091,
    343285649282301
  ]

visaNumbers :: [Integer]
visaNumbers =
  [ 4539852937043376,
    4556891100168592,
    4539244287039024,
    4532804927982144,
    4532128203765076,
    4556470109947841,
    4485995435062085,
    4929989284890336,
    4539136810768618,
    4024007166444900,
    4916579626626,
    4929427073677,
    4532157995026,
    4790456235656,
    4929637065067,
    4716732725889,
    4716432786298,
    4556536072199,
    4916917451893,
    4916552422373
  ]

masterCardNumbers :: [Integer]
masterCardNumbers =
  [ 5246904813208432,
    5478071256875322,
    5545568850727998,
    5474943796770789,
    5521568871516345,
    5264035731689838,
    5432921709090231,
    5269841959362767,
    5322307766616857,
    5323792998781984,
    5224854736521666,
    2222420000001113,
    2223000048410010
  ]

-- Implementing QuickCheck to finally test the correctness of our Luhn algorithm function.
exercise7 :: IO ()
exercise7 = do
  putStrLn $ "Test American Express: " ++ show (testIsAmericanExpress americanExpressNumbers)
  putStrLn $ "Test Master Cards: " ++ show (testIsMaster masterCardNumbers)
  putStrLn $ "Test Visa: " ++ show (testIsVisa visaNumbers)

-- {Time spent}: 90 minutes