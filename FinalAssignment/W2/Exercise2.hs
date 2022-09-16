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
  a > 0
    && b > 0
    && c > 0
    && a + b > c
    && a + c > b
    && b + c > a

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c =
  pythagoreanTheorem a b c
    || pythagoreanTheorem c a b
    || pythagoreanTheorem b c a

pythagoreanTheorem :: Integer -> Integer -> Integer -> Bool
pythagoreanTheorem a b c = (a ^ 2) + (b ^ 2) == (c ^ 2)

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

prop_anySideLargerThenSumOfSides :: Positive Integer -> Positive Integer -> Positive Integer -> Property
prop_anySideLargerThenSumOfSides (Positive a) (Positive b) (Positive c) = a + b < c || a + c < b || b + c < a ==> triangle a b c == NoTriangle

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
-- Because of overlap with an equilateral triangle, we use the same integer for all sides and append 1 to one side.
prop_isIsosceles :: Positive Integer -> Property
prop_isIsosceles (Positive a) = isValidTriangle a a (a + 1) ==> triangle a a (a + 1) == Isosceles

-- We can generate other triangles by constraining the input on being a valid triangle and
-- not being a rectangular, equilateral and isosceles triangle
prop_isOther :: Positive Integer -> Positive Integer -> Positive Integer -> Property
prop_isOther (Positive a) (Positive b) (Positive c) =
  isValidTriangle a b c
    && not (isRectangular a b c)
    && not (isEquilateral a b c)
    && not (isIsosceles a b c) ==> triangle a b c == Other

main :: IO ()
main = do
  putStrLn "\bExercise 7\nTime spent +/- 45 minutes\n"
  
  putStrLn
    "To check the correctness of the program we implemented different tests \
    \based on the different properties of the different triangles\n"

  putStrLn
    "A triangle is a valid triangle, If and only If, \
    \the sum of any two sides of a triangle is greater than the third side. \
    \If the input contains a side that is larger than the sum of the other two side, the program should return 'NoTriangle'"
  quickCheck prop_anySideLargerThenSumOfSides
  putStrLn ""

  putStrLn "A triangle can't have any negative sides, if the input has a negative side, the program should return 'NoTriangle'"
  quickCheck prop_negativeSides
  putStrLn ""

  putStrLn
    "Using Pythagoram theorem (a^2 + b^2 = c^) we can calculate a rectangular triangle. \
    \We found that 3^2 + 4^2 = 5^2, so we can use 'n * 3^2 + n * 4^2 = n * 5^2' to generate a list of rectangular triangles."
  quickCheck prop_isRectangular
  putStrLn ""

  putStrLn
    "A triangle is only an equilateral triangle if every side has the same size (a == b == c). \
    \By using the same positive integer a as side, we assure that every side has the same length."
  quickCheck prop_isEquilateral
  putStrLn ""

  putStrLn
    "An isosceles triangle is a triangle that has at least two sides of equal length (a == b || a == c || b == c). \
    \Because of overlap with an equilateral triangle, we use the same integer for all sides and append 1 to one side."
  quickCheck prop_isIsosceles
  putStrLn ""

  putStrLn
    "We can generate other triangles by constraining the input on being a valid triangle and \
    \not being a rectangular, equilateral and isosceles triangle"
  quickCheck prop_isOther