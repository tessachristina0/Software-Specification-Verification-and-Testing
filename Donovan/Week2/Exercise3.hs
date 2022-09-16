-- Donovan Schaafsma
-- Exercise 3
module Exercise3 where

import Data.List (sortBy)

infix 1 -->

{-
Given functions from the exercise.
-}
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)
weaker xs p q = stronger xs q p

{-
The properties were given in the Excercise. In the properties we also explicitly defined propertyEven because of readability and completeness. 
-}
propertyEven :: Int -> Bool
propertyEven = even

propertyEvenAndGreaterThanThree :: Int -> Bool
propertyEvenAndGreaterThanThree x = even x && x > 3

propertyEvenOrGreaterThanThree :: Int -> Bool
propertyEvenOrGreaterThanThree x = even x || x > 3

propertyEvenAndGreaterThanThreeOrEven :: Int -> Bool
propertyEvenAndGreaterThanThreeOrEven x = (even x && x > 3) || even x

-- This function returns the correct property based on the string parameter input
setPropertyList :: String -> Int -> Bool
setPropertyList p x = if p == "propertyEven" then propertyEven x
    else if p == "propertyEvenAndGreaterThanThree" then propertyEvenAndGreaterThanThree x
    else if p == "propertyEvenOrGreaterThanThree" then propertyEvenOrGreaterThanThree x
    else (p == "propertyEvenAndGreaterThanThreeOrEven") && propertyEvenAndGreaterThanThreeOrEven x

{- 
This was a very hard assignment since we were contemplating multiple ways of implementing the descending list. We were first looking into ways to recursively compare lists to one another to find the strongest property.
However this approach became complicated very quickly, hence why the assingment cost some serious hours. 
Fortunately Haskell provides the sortBy function which made the comparing of the properties possible without us needing to use recursion (it is however possible that sortBy uses recursion under the hood).

This function takes two properties as parameter input, applies the stronger function and gives a property a score of GT or LT. 
Based on how strong the property is compared to the other property in the input. 

This function combined with the setPropertyList function can then be combined for the sortBy function that Haskell provides. 
This gives the desired output for properties based on strength in a descending order. 
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