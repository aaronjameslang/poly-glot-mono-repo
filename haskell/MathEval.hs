-- -- Evaluate Mathematical Expression

-- A 2nd kyu problem posed by codewars.com:
-- https://www.codewars.com/kata/52a78825cdfc2cfc87000005

-- Given a mathematical expression as a string you must return the result as a number.

-- Numbers may be either a whole numbers and/or decimal numbers.

-- You need to support the following mathematical operators:
--     Multiplication *
--     Division / (as floating point division)
--     Addition +
--     Subtraction -

-- Operators are always evaluated from left-to-right, and * and / must be evaluated before + and -.

-- You need to support multiple levels of nested parentheses, ex. (2 / (2 + 3.33) * 4) - -6

import Data.Char (isDigit)
import Data.Foldable (for_)
import Data.Function ((&))
import GHC.Unicode (isSpace)
import Test.Hspec
import Text.Read (readMaybe)

main = hspec $ do
  describe "example tests" $ do
    for_
      [ ("1", 1)
      , ("1+1", 2)
      , ("1+1+1", 3)
      , ("1+1 + 1", 3)
      , ("1 - 1", 0)
      , ("1* 1", 1)
      , ("1 /1", 1)
      , ("-123", -123)
      , ("123", 123)
      , ("4.75- -6", 10.75)
      , ("2 /2+3 * 4.75- -6", 21.25)
      , ("12* 123", 1476)
      , ("12*-1", -12)
      , ("(1)", 1)
      , ("2*(3+5)", 16)
      , ("2 / (2 + 3) * 4.33 - -6", 7.732)
      , ("-(1)", -1)
      , ("12* 123/-(-5 + 2)", 492)
      , ("89.94/(67.61)-(40.25+(11.49))+(98.41)/((75.76/(18.26))*(41.73)/(23.24)*95.62)+92.68+-8.56*77.11/(--61.46*-2.79*(87.35)-71.62*73.35-67.57+11.24*27.72)*7.38*10.7*20.13*78.89/((73.45/(57.52+76.61+98.37-53.13)))/((-11.98)-(4.44))", -573.500021609947)
      , ("(--54.51-(26.99)+33.64/(58.27))+--73.46/(24.61)*8.42+59.77/(79.21)-(--98.81+43.06)*--81.7-25.79+-86.76-55.77-55.89/(48.11)*74.69+-31.25/(4.23+((84.76*35.01+75.42)*66.01)*46.59*29.67-97.84-16.6/(69.75*-45.97))", -11791.882026648824)
      ]
      $ \(arg, expected) -> do
        it (show arg) $ do
          calc arg `shouldBe` expected

calc :: String -> Double
calc s =
  s
    & tokenise
    & evalExpression

---------------------------------------
-- Tokenisation

data Token = TN Double | TC Char deriving (Show)

tokenise :: String -> [Token]
tokenise "" = []
tokenise (c : cs)
  | isSpace c = tokenise cs
  | isDigit c =
      let (token, str) = tokeniseNumericPrefix (c : cs)
       in token : tokenise str
  | otherwise = TC c : tokenise cs

tokeniseNumericPrefix :: String -> (Token, String)
tokeniseNumericPrefix s =
  let (prefix, rest) = span (\x -> isDigit x || x == '.') s
   in (tokeniseNumericString prefix, rest)

tokeniseNumericString :: String -> Token
tokeniseNumericString s =
  let mn = readMaybe s
   in case mn of
        Just n -> TN n
        Nothing -> error $ "Could not parse number: \"" ++ s ++ "\""

---------------------------------------
-- Evaluation

evalExpression :: [Token] -> Double
evalExpression ts =
  let (n, ts') = evalNextExpression ts
   in case ts' of
        [] -> n
        _ -> error $ "Trailing tokens: " ++ show ts'

-- Expression are composed of terms that are added or subtracted
evalNextExpression :: [Token] -> (Double, [Token])
evalNextExpression ts =
  let (termX, ts') = evalNextTerm ts
   in case ts' of
        -- If end of expression, return what we have
        [] -> (termX, [])
        TC ')' : _ -> (termX, ts')
        -- If expression continues, evaluate the next term and combine
        TC op : ts'' ->
          -- op is '+' or '-'
          let (termY, ts''') = evalNextTerm ts''
              expr = combineTerms termX op termY
           in evalNextExpression $ TN expr : ts'''

combineTerms :: Double -> Char -> Double -> Double
combineTerms x '+' y = x + y
combineTerms x '-' y = x - y

-- Terms are things that are added or subtracted,
-- they are composed of factors that are multiplied or divided
evalNextTerm :: [Token] -> (Double, [Token])
evalNextTerm ts =
  let (factorX, ts') = evalNextFactor ts
   in case ts' of
        -- If end of term, return what we have
        [] -> (factorX, [])
        TC '+' : _ -> (factorX, ts')
        TC '-' : _ -> (factorX, ts')
        TC ')' : _ -> (factorX, ts')
        -- If term continues, evaluate the next factor and combine
        TC op : ts'' ->
          -- op is '*' or '/'
          let (factorY, ts''') = evalNextFactor ts''
              term = combineFactors factorX op factorY
           in evalNextTerm $ TN term : ts'''

combineFactors :: Double -> Char -> Double -> Double
combineFactors x '*' y = x * y
combineFactors x '/' y = x / y

-- Factors are things that are multiplied or divided,
-- they can numbers, negative numbers, or sub-expressions in parentheses
evalNextFactor :: [Token] -> (Double, [Token])
evalNextFactor (TN n : ts) = (n, ts)
evalNextFactor (TC '-' : ts) =
  let (n, ts') = evalNextFactor ts
   in (-n, ts')
evalNextFactor (TC '(' : ts) =
  let (n, ts') = evalNextExpression ts
   in case ts' of
        TC ')' : ts'' -> (n, ts'')
        _ -> error "Unmatched parentheses"
evalNextFactor other = error $ "Invalid next factor: " ++ show other
