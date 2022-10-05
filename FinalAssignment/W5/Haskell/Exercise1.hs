-- SSVT Haskell Lab
-- Week 5 - Group 5
-- Exercise 1: We provide some mutators to mutate the output of the list in Mutation.hs. Write down which types of output are not yet covered by these mutators, 
-- and about their weakness/strength. Come up with a list of other mutators and implement (a subset of) them.
-- Deliverables: List of mutators and rationale, implementation, indication of time spent.
-- Time spend: ~ hours --
module FinalAssignment.W5.Haskell.Exercise1 where
import FinalAssignment.W5.Haskell.Mutation
import FinalAssignment.W5.Haskell.MultiplicationTable
import Test.QuickCheck
import Data.List

{- 
List of mutators and rationale:
The mutation functions which are applied in Mutation.hs are: 
  - addElements 
  - removeElements

Not covered per mutator:
  addElements:
    - Random additions in the list before the existing array. Now there is always one new integer in front of the existing array.
    - List given in order. The list isn't ordered now, it is completely random.
  removeElements:
    - Not covered: Random deletion, it always removes from right to left in a list
    - Not covered: The first item is never removed from the list

Both mutators are weak since they are not covering a lot of constraints.

List of other possible mutators (there can be more):
  - Arithmetic: Add/Substract/Multiply/Divide/Modulo
  - Array Declaration​: Empty a list ([1,2,3,4] -> [])
  - Boolean Literal: Change the value of the boolean
  - Equality Operator: Change the equality operator in a function
  - Logical Operator: Change the logical operator in a function to a different logical operator
-}

-- Function to add a random generated number to each element in the list and return a new list with the new values.
addition :: [Integer] -> Gen [Integer]
addition xs = do
    num <- arbitrary :: Gen Integer
    if  fmap ((+) num) xs /= xs
    then return (fmap ((+) num) xs)
    else addition xs

-- Function to subtract a random generated number to each element in the list and return a new list with the new values.
subtraction :: [Integer] -> Gen [Integer]
subtraction xs = do
    num <- arbitrary :: Gen Integer
    if fmap (subtract num) xs /= xs
    then return (fmap (subtract num) xs)
    else subtraction xs

-- Function to multiply a random generated number to each element in the list and return a new list with the new values.
multiplication :: [Integer] -> Gen [Integer]
multiplication xs = do
    num <- arbitrary :: Gen Integer
    if fmap ((*) num) xs /= xs
    then return (fmap ((*) num) xs)
    else multiplication xs

-- Function to divide a random generated number to each element in the list and return a new list with the new values.
divide :: [Integer] -> Gen [Integer]
divide xs = do
    num <- arbitrary :: Gen Integer
    if fmap (div num) xs /= xs
    then return (fmap (div num) xs)
    else divide xs

-- Function to call modulo with a random generated number to each element in the list and return a new list with the new values.
modulo :: [Integer] -> Gen [Integer]
modulo xs = do
    num <- arbitrary :: Gen Integer
    if fmap (mod num) xs /= xs
    then return (fmap (mod num) xs)
    else modulo xs
