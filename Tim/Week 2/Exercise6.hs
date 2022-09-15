-- Tim van Ekert
-- Exercise 6
module Exercise6 where
import Data.Maybe (fromMaybe)
import Data.Char ( ord, chr, toUpper )
import Test.QuickCheck (Property, quickCheck, (==>))

{- 
Specification of rot13: can be said as rotation 13, says actually what it does.
There are 26 characters in the english alphabet, which is splitted in to two parts of 13 characters.
The first step is to get the index of the first letter and get the addition or minus of 13 of that letter.
That means the word is rotated and can be rotated back 13 indexes.
-}

rot13 :: Char -> Char
rot13 c
  | toUpper c `elem` ['A' .. 'M'] = chr $ ord c + 13
  | toUpper c `elem` ['N' .. 'Z'] = chr $ ord c - 13
  | otherwise = c

testingString :: String -> [String]
testingString s = [s, map rot13 s, map (rot13 . rot13) s]


-- Quicktest properties
prop_is_inversed :: [Char] -> Bool
prop_is_inversed s = s == map (rot13 . rot13) s

prop_only_alphabetic :: [Char] -> Bool
prop_only_alphabetic s = map rot13 (filter (\x -> x `notElem` ['A' .. 'Z'] && x `notElem` ['a' .. 'z']) s) == 
                              filter (\x -> x `notElem` ['A' .. 'Z'] && x `notElem` ['a' .. 'z']) s

exercise6 :: IO ()
exercise6 = do
  print(testingString "Testing this string")
  quickCheck prop_is_inversed
  quickCheck prop_only_alphabetic
