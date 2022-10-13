-- SSVT Haskell Lab
-- Week 6 - Group 5
-- Exercise 4: Function for checking whether a relation is serial
-- Deliverables: Haskell program, QuickCheck properties, short test report (including the proof), indication of time spent.
-- Time spend: ? minutes -

module FinalAssignment.W6.Exercise4 where
import FinalAssignment.W6.SetOrd
import Data.List
import System.Random
import Test.QuickCheck
import Control.Monad
import Test.QuickCheck.Test
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Data.Map.Internal.Debug

{-
A relation R is serial on a domain A if for any x ∈ A there is an y ∈ A such that xRy. 
Suppose relations are represented as lists of pairs:
"type Rel a = [(a,a)]"

That is, a relation R ⊆ 𝑆×𝑆 is serial if and only if *every element* of 𝑆 relates to *some* other element of 𝑆.
-}

-- A) Write a function for checking whether a relation is serial:
type Rel a = [(a,a)]

-- ALGORITHM -- TODO: finish in ENG
-- 1) Check recursief of x in [(a,b)] zit (dus x == a of x == b). 
-- 2) Check erna of ...
-- lijst van x1, x2, x3 etc.
-- lijst van relaties (a,b)
-- om te weten of relatie (a,b) valid is, check je of ELK element van lijst x1, x2 etc. 
-- tevens in de lijst van relaties (a,b) zit. 


isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial ds rs = and [ d `elem` rList rs | d <- ds ] && and [r `elem` ds | r <- rList rs]



-- B) Test your implementation with two QuickCheck properties:

-- Generator to create the domain with maximum length
genDomain :: Gen [Integer]
genDomain = (arbitrary :: Gen [Integer]) `suchThat` (\x -> length x <= 8 && x /= [])

-- Generator to create and return the domain with Relation consisting of 10 tuples
genRel :: Gen ([Integer], Rel Integer)
genRel = do
    ds <- genDomain
    rela <- vectorOf 10 (genTuple ds)
    return (ds, rela)

-- Create a relation-tuple from a given domain
genTuple :: [b] -> Gen (b, b)
genTuple ds = do
    x <- elements ds
    y <- elements ds
    return (x,y)

-- Generator to create and return the domain with non-serial Relation
genRelInv :: Gen ([Integer], Rel Integer)
genRelInv = do
    ds <- genDomain
    xs <- genDomain
    rela <- vectorOf 10 (genTuple xs)
    return (ds, rela)


-- Property 1: Every element in tuples of the relation should make up *whole* domain of elements.
-- Note: not reverse checking, but checking result to original domain.
-- Q: Does every element of R (rs, the list of R) need to be in domain???

-- Make list of all elements in tuples of relations
rList :: Rel a -> [a]
rList rs = [  x | (x,_) <- rs ] ++ [ y | (_,y) <- rs ]

-- For checking if both lists contain the same elements, thus are equal lists. 
-- Inspired by: https://www.reddit.com/r/haskell/comments/q0yj7h/how_can_i_check_if_two_lists_in_haskell_have_the/
subset :: Eq a => [a] -> [a] -> Bool
subset a b = all (`elem` b) a

prop_domain :: Eq a => [a] -> Rel a -> Bool
prop_domain ds rs = isSerial ds rs && (subset ds (rList rs) && subset (rList rs) ds)

-- Property 2: Minimal length of relation is length of the domain.
-- Q: Are there potentially duplicates, and if so: Do they need to be removed to check lengths?
prop_minLength :: Eq a => [a] -> Rel a -> Bool
prop_minLength ds rs = isSerial ds rs && (length (rList rs) >= length ds)


-- QuickCheck: Serial relations
serialQuickCheck :: IO ()
serialQuickCheck = do
    quickCheck $ forAll genRel (uncurry prop_domain)
    quickCheck $ forAll genRel (uncurry prop_minLength)
-- Notes: Uncurry filters the input for prop_domain before call of the function. The input for prop_ is given by the generator genRel, 
-- and passed on by forAll. forAll takes the created generator as its input and repeats it a number of times for many cases of 
-- testing with QuickCheck. 


-- quickcheck genRel' uncurry isSerial
--            (a,b)            isSerial a 


-- C) Consider the relation R = {(x, y) | x = y(mod n)}, where (mod n) is the modulo function in modular arithmetic and n > 0. 
-- Discuss whether (and when) R is serial. 
{- 
Modulo divides one number by another as many times as possible and gives the rest-number. Thus, in (x, y), y will always be divided by
some n and what is left (or: in case n > y, the whole y will be left such that x == y) is x. 


Suppose we have some n, say n = 7, and would have some relations. Then the relations would for example be: {(1, 8), (2,9), (9, 18), (8, ...), ...}.
This is as you can see already not valid, given that for (1,8) there should be a relation (8, _) - but this is impossible given that 8 > 7 
so could not be 8 (but only 8-7 = 1). Same counts for (2,9) with (9,_).

Relation R could potentially be serial if, and only if:
1) x would be equal to y. This way, every x would have a relation to some y from the domain - namely to itself.
2) n would be larger than the largest number in the domain. This makes point 1 possible, for instance {(1,1), (2,2), (3,3), ...}, where
n has no influence on the relations.
3) Since x == y, *all* the numbers y from the relations should make up the *whole* domain. 
-}

-- How can you test whether R is serial? 
{-
We can test this by creating a function such as isSerial. The above defined isSerial may be adapted to include the above conditions,
such as:
-}

isSerialC :: Eq a => [a] -> Rel a -> Bool
isSerialC ds rs = and [ d `elem` rList rs | d <- ds ] && and [r `elem` ds | r <- rList rs] && all (uncurry (==)) rs

{- 
Here, the R = {(x, y) | x = y(mod n)} could also be checked by adding in an extra function checking for n, like:

checkN :: Eq a => [a] -> Rel a -> Bool
checkN ds rs = and [ x == y `mod` n | (x,y) <- rs ] 
    where 
        n = (ds !! (length (ordered ds) - 1)) + 3
-}

-- How can you prove that R is serial?
{-
We can proof this by making proporty tests with the above conditions 1, 2 and 3. Then the properties may be tested upon the previous serial
testing-function with for instance QuickCheck.
E.g.:
1) x == y for every (x,y) in R
2) n > ds !! (length (ordered ds) - 1)
3) ...
-}