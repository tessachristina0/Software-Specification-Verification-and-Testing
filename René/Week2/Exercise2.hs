import Test.QuickCheck (Negative (Negative), Positive (Positive), Property, quickCheck, (==>))

data Shape
  = NoTriangle
  | Equilateral
  | Isosceles
  | Rectangular
  | Other
  deriving (Eq, Show)

-- A triangle is only valid if the sum of two sides is larger then the other side.
isValidTriangle :: Integer -> Integer -> Integer -> Bool
isValidTriangle a b c =
  a + b > c
    && a + c > b
    && b + c > a
    && a > 0
    && b > 0
    && c > 0

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c =
  isRectangular' a b c
    || isRectangular' c a b
    || isRectangular' b c a

-- A rectular triangle can be calculated by taking the square root of the sum of two squared sides
-- and comparing it with the other side. It it a rectular triangle if any two sides match with the other side.
isRectangular' :: Integer -> Integer -> Integer -> Bool
isRectangular' a b c = sqrt (fromIntegral ((a ^ 2) + (b ^ 2))) == fromIntegral c

isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = a == b && a == c

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c =
  a == b
    || a == c
    || b == c

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
  | not $ isValidTriangle a b c = NoTriangle
  | isRectangular a b c = Rectangular
  | isEquilateral a b c = Equilateral
  | isIsosceles a b c = Isosceles
  | otherwise = Other

prop_oneSideLargerThenSumOfSides :: Positive Integer -> Positive Integer -> Positive Integer -> Property
prop_oneSideLargerThenSumOfSides (Positive a) (Positive b) (Positive c) = a + b < c || a + c < b || b + c < a ==> triangle a b c == NoTriangle

prop_negativeSides :: Negative Integer -> Negative Integer -> Negative Integer -> Bool
prop_negativeSides (Negative a) (Negative b) (Negative c) = triangle a b c == NoTriangle

-- Using Pythagoram theorem (a^2 + b^2 = c^) we can calculate a rectangular triangle.
-- We found that 3^2 + 4^2 = 5^2, so we can use "n * 3^2 + n * 4^2 = n * 5^2" to generate a list of rectangular triangles.
prop_isRectangular :: Positive Integer -> Bool
prop_isRectangular (Positive a) = triangle (a * 3) (a * 4) (a * 5) == Rectangular

-- A triangle is only an equilateral triangle if every side has the same size (a == b == c).
-- By using the same positive integer a as side, we assure that every side has the same length.
prop_isEquilateral :: Positive Integer -> Bool
prop_isEquilateral (Positive a) = triangle a a a == Equilateral

-- An isosceles triangle is a triangle that has at least two sides of equal length (a == b || a == c || b == c).
-- Because of overlap with an equilateral triangle, we use the same integer for all sides and append 1 to the
prop_isIsosceles :: Positive Integer -> Property
prop_isIsosceles (Positive a) = isValidTriangle a a (a + 1) ==> triangle a a (a + 1) == Isosceles

prop_isOther :: Positive Integer -> Positive Integer -> Positive Integer -> Property
prop_isOther (Positive a) (Positive b) (Positive c) =
  isValidTriangle a b c
    && not (isRectangular a b c)
    && not (isEquilateral a b c)
    && not (isIsosceles a b c) ==> triangle a b c == Other

exercise2 :: IO ()
exercise2 = do
  quickCheck prop_oneSideLargerThenSumOfSides
  quickCheck prop_negativeSides
  quickCheck prop_isRectangular
  quickCheck prop_isEquilateral
  quickCheck prop_isIsosceles
  quickCheck prop_isOther