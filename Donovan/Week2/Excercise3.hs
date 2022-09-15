infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

propertyList :: [Int -> Bool]
propertyList = [propertyEven, propertyEvenAndGreaterThanThree, propertyEvenOrGreaterThanThree, propertyEvenAndGreaterThanThreeOrEven]

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

propertyEven :: Int -> Bool
propertyEven = even

propertyEvenAndGreaterThanThree :: Int -> Bool
propertyEvenAndGreaterThanThree x = even x && x > 3

propertyEvenOrGreaterThanThree :: Int -> Bool
propertyEvenOrGreaterThanThree x = even x || x > 3

propertyEvenAndGreaterThanThreeOrEven :: Int -> Bool
propertyEvenAndGreaterThanThreeOrEven x = (even x && x > 3) || even x

--Consider a small domain like [(−10)..10]
-- [(-5),(-4),(-3),(-2),(-1),0,1,2,3,4,5]

test1 :: Bool
test1 = stronger [(-10)..10] propertyEvenAndGreaterThanThree propertyEven
test2 :: Bool
test2 = stronger [(-10)..10] propertyEvenOrGreaterThanThree propertyEven
test3 :: Bool
test3 = stronger [(-10)..10] propertyEvenAndGreaterThanThreeOrEven propertyEven
test4 :: Bool
test4 = stronger [(-10)..10] propertyEven propertyEvenAndGreaterThanThreeOrEven

-- strongest :: (Enum a, Num a) => [a] -> [a] -> [Bool]
-- strongest l1 l2 = [stronger [(−10)..10] l1 l2]

-- exercise2 :: IO()
-- exercise2 = putStrLn "HOI" -- TODO: How to test this?