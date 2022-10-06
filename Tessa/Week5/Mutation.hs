module FinalAssignment.W5.Haskell.Mutation where
import Test.QuickCheck
import Data.List
import Data.Maybe

--                  Mutator                     Property                      Function Under Test     Input       
mutate :: ([Integer] -> Gen [Integer]) -> ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> Integer -> Gen (Maybe Bool)
mutate mutator prop fut input = mutation >>= \mutant -> mutateOrNothing output mutant (Just <$> propertyExecutor prop mutation input)
        where output = fut input
              mutation = mutator output
--                          Does the property hold on the output (namely the output of the Function Under Test on input)?



mutateOrNothing :: [Integer] -> [Integer] -> Gen (Maybe Bool) -> Gen (Maybe Bool)
mutateOrNothing output mutant res | output == mutant = return Nothing
                                  | otherwise = res

-- Compares 
propertyExecutor :: ([Integer] -> Integer -> Bool) -> Gen [Integer] -> Integer -> Gen Bool
propertyExecutor prop o x = o >>= \output -> return $ prop output x
--                          Take elem of o (Gen [Int]) and use as arg "output". Output is taken as first arg for Property-function "prop"
--                          while Integer x is taken as second arg, and a Gen Bool is returned.

-- Mutators
addElements :: [Integer] -> Gen [Integer]
addElements xs = do
  nums <- arbitrary :: Gen [Integer]
  num <- arbitrary :: Gen Integer
  return $ num : xs ++ nums

removeElements :: [Integer] -> Gen [Integer]
removeElements xs = choose (1, length xs - 1) >>= \x -> return $ take x xs
