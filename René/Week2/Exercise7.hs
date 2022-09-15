import Data.Char (isDigit, ord)
import Data.List (find)

type CountryCode = String

type AmountOfChars = Int

validIbans :: [(AmountOfChars, CountryCode)]
validIbans =
  [ (28, "AL"),
    (24, "AD"),
    (20, "AT"),
    (28, "AZ"),
    (22, "BH"),
    (28, "BY"),
    (16, "BE"),
    (20, "BA"),
    (29, "BR"),
    (22, "BG"),
    (22, "CR"),
    (21, "HR"),
    (28, "CY"),
    (24, "CZ"),
    (18, "DK"),
    (28, "DO"),
    (23, "TL"),
    (29, "EG"),
    (28, "SV"),
    (20, "EE"),
    (18, "FO"),
    (18, "FI"),
    (27, "FR"),
    (22, "GE"),
    (22, "DE"),
    (23, "GI"),
    (27, "GR"),
    (18, "GL"),
    (28, "GT"),
    (28, "HU"),
    (26, "IS"),
    (23, "IQ"),
    (22, "IE"),
    (23, "IL"),
    (27, "IT"),
    (30, "JO"),
    (20, "KZ"),
    (20, "XK"),
    (30, "KW"),
    (21, "LV"),
    (28, "LB"),
    (25, "LY"),
    (21, "LI"),
    (20, "LT"),
    (20, "LU"),
    (19, "MK"),
    (31, "MT"),
    (27, "MR"),
    (30, "MU"),
    (27, "MC"),
    (24, "MD"),
    (22, "ME"),
    (18, "NL"),
    (15, "NO"),
    (24, "PK"),
    (29, "PS"),
    (28, "PL"),
    (25, "PT"),
    (29, "QA"),
    (24, "RO"),
    (32, "LC"),
    (27, "SM"),
    (25, "ST"),
    (24, "SA"),
    (22, "RS"),
    (31, "SC"),
    (24, "SK"),
    (19, "SI"),
    (24, "ES"),
    (18, "SD"),
    (24, "SE"),
    (21, "CH"),
    (24, "TN"),
    (26, "TR"),
    (29, "UA"),
    (23, "AE"),
    (22, "GB"),
    (22, "VA"),
    (24, "VG")
  ]

iban :: String -> Bool
iban s = correctLength s (findMatchingCountry s) && computeRemainder (convertToInteger $ replaceLetters $ rearrange s) == 1

correctLength :: String -> Maybe (AmountOfChars, CountryCode) -> Bool
correctLength str tuple = case tuple of
  Just (l, country) -> length str == l
  Nothing -> False

findMatchingCountry :: String -> Maybe (AmountOfChars, CountryCode)
findMatchingCountry [] = Nothing
findMatchingCountry [_] = Nothing
findMatchingCountry (x : y : xs) = find (\(n, ib) -> [x, y] == ib) validIbans

replaceLetters :: [Char] -> [Char]
replaceLetters [] = []
replaceLetters (x : xs)
  | isDigit x = x : replaceLetters xs
  | otherwise = show (ord x - 55) ++ replaceLetters xs

rearrange :: [Char] -> [Char]
rearrange [] = []
rearrange [_] = []
rearrange [_, _] = []
rearrange [_, _, _] = []
rearrange [_, _, _, _] = []
rearrange (a : b : c : d : xs) = xs ++ [a, b, c, d]

convertToInteger :: String -> Integer
convertToInteger s = read s :: Integer

computeRemainder :: Integer -> Integer
computeRemainder n = n `mod` 97
