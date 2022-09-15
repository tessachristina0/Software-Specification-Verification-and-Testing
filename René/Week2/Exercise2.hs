data Shape
  = NoTriangle
  | Equilateral
  | Isosceles
  | Rectangular
  | Other
  deriving (Eq, Show)

isValidTriangle :: Integer -> Integer -> Integer -> Bool
isValidTriangle a b c = a + b > c && a + c > b && b + c > a

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = isRectangular' a b c || isRectangular' c a b || isRectangular' b c a

isRectangular' :: Integer -> Integer -> Integer -> Bool
isRectangular' a b c = sqrt (fromIntegral ((a ^ 2) + (b ^ 2))) == fromIntegral c

isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = a == b && a == c

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c = a == b || a == c || b == c
 
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
  | not $ isValidTriangle a b c = NoTriangle
  | isEquilateral a b c = Equilateral
  | isRectangular a b c = Rectangular
  | isIsosceles a b c = Isosceles
  | otherwise = Other

exercise2 :: IO()
exercise2 = putStrLn "HOI" -- TODO: How to test this?