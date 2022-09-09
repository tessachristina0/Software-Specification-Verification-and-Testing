forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

toDigitArray :: Integer -> [Integer]
toDigitArray 0 = [0]
toDigitArray x = toDigitArray (x `div` 10) ++ [x `mod` 10]

digitArrayToInteger :: [Integer] -> Integer
digitArrayToInteger = read . concatMap show

doubleEverySecondDigit :: [Integer] -> [Integer]
doubleEverySecondDigit [] = []
doubleEverySecondDigit l = [if even i then addDigitsOfProduct (v * 2) else v | (i, v) <- zip [1 ..] l]

addDigitsOfProduct :: Integer -> Integer
addDigitsOfProduct n
  | n > 9 = sum $ toDigitArray n
  | otherwise = n

luhn :: Integer -> Bool
luhn n = sum (doubleEverySecondDigit $ toDigitArray $ reversal n) `mod` 10 == 0

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress n =
  x == 3 && (y == 4 || y == 7)
    && length (x : y : xs) == 15
    && luhn n
  where
    (x : y : xs) = toDigitArray n
isVisa n =
  x == 4
    && (length (x : xs) >= 13 || length (x : xs) <= 10)
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