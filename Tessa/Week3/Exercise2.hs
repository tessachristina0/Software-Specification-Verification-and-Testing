module Exercise2 where
import Data.List
import System.Random
import Test.QuickCheck
import Tessa.Week3.Lecture3


-- Simplest parser:
succeed :: b -> Parser a b
succeed x xs = [(x,xs)]

-- Parser implementation in your lab:
parseForm :: Parser Token Form


data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form
          | Equiv Form Form
          deriving (Eq,Ord)


parse :: String -> [Form]
lexer :: String -> [Token]
parseForm :: Parser Token Form


