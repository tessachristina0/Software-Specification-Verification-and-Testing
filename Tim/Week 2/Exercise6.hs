-- Tim van Ekert
-- Exercise 6
module Exercise6 where
import Data.Maybe (fromMaybe)
import Data.Char ( ord, chr, toUpper )
import Test.QuickCheck (Property, quickCheck, (==>))

{- 
Specification of rot13: can be said as rotation 13, says actually what it does.
There are 26 characters in the English alphabet, split into two parts of 13 characters.
The first step is to get the index of the first letter and the addition or minus of 13 of that letter.
That means the word is rotated and can be rotated back to 13 indexes.
-}

-- Function which does the actual rotation algorithm. For the letters A..M, it adds 13 to the actual index. And for the letters N..Z, it subtracts 13 from the existing index.
rot13 :: Char -> Char
rot13 c
  | toUpper c `elem` ['A' .. 'M'] = chr $ ord c + 13
  | toUpper c `elem` ['N' .. 'Z'] = chr $ ord c - 13
  | otherwise = c

-- Function to actually check the given string, rotated string and the rotation of the rotated string in an array next to each other.
testingString :: String -> [String]
testingString s = [s, map rot13 s, map (rot13 . rot13) s]

-- Quicktest properties
-- Property 1: For every string, compare the actual string with the inversion of the inversion. So it should be the same string again.
prop_is_inversed :: [Char] -> Bool
prop_is_inversed s = s == map (rot13 . rot13) s

-- Property 2: For every string filter the A..Z or a..z out of the string. Then inverse the string to check if the string is the same.
-- Because when there are other characters used then from the english alphabet the string should stay the same.
prop_only_alphabetic :: [Char] -> Bool
prop_only_alphabetic s = map rot13 (filter (\x -> x `notElem` ['A' .. 'Z'] && x `notElem` ['a' .. 'z']) s) == 
                              filter (\x -> x `notElem` ['A' .. 'Z'] && x `notElem` ['a' .. 'z']) s

exercise6 :: IO ()
exercise6 = do
  print(testingString "Testing this string")
  quickCheck prop_is_inversed
  quickCheck prop_only_alphabetic
