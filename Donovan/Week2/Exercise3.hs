-- Donovan Schaafsma
-- Exercise 3
module Exercise3 where

import Data.List (sortBy)

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)
weaker xs p q = stronger xs q p

{-
The properties were given in the Excercise. In the properties we also explicitly defined propertyEven because it is easier to work with
that way. 
-}
propertyEven :: Int -> Bool
propertyEven = even

propertyEvenAndGreaterThanThree :: Int -> Bool
propertyEvenAndGreaterThanThree x = even x && x > 3

propertyEvenOrGreaterThanThree :: Int -> Bool
propertyEvenOrGreaterThanThree x = even x || x > 3

propertyEvenAndGreaterThanThreeOrEven :: Int -> Bool
propertyEvenAndGreaterThanThreeOrEven x = (even x && x > 3) || even x

{-
TODO:
-}
setPropertyList :: String -> Int -> Bool
setPropertyList p x =if p == "propertyEven" then propertyEven x
    else if p == "propertyEvenAndGreaterThanThree" then propertyEvenAndGreaterThanThree x
    else if p == "propertyEvenOrGreaterThanThree" then propertyEvenOrGreaterThanThree x
    else (p == "propertyEvenAndGreaterThanThreeOrEven") && propertyEvenAndGreaterThanThreeOrEven x

{-
TODO:
-}
sortPropList :: String -> String -> Ordering
sortPropList x y =
  if stronger [-10 .. 10] (setPropertyList x) (setPropertyList y)
    then GT
    else LT

exercise3 :: IO ()
exercise3 = do
  print
    ( sortBy
        sortPropList
        [ "propertyEven",
          "propertyEvenAndGreaterThanThree",
          "propertyEvenOrGreaterThanThree",
          "propertyEvenAndGreaterThanThreeOrEven"
        ]
    )

-- Time indication: 4 hours