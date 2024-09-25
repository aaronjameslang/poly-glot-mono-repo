-- -- Evaluate Mathematical Expression

-- A 2nd kyu problem posed by codewars.com:
-- https://www.codewars.com/kata/52a78825cdfc2cfc87000005

-- Given a mathematical expression as a string you must return the result as a number.

-- Number may be both whole numbers and/or decimal numbers. The same goes for the returned result.

-- You need to support the following mathematical operators:

--     Multiplication *
--     Division / (as floating point division)
--     Addition +
--     Subtraction -

-- Operators are always evaluated from left-to-right, and * and / must be evaluated before + and -.

-- You need to support multiple levels of nested parentheses, ex. (2 / (2 + 3.33) * 4) - -6

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (elemIndex, elemIndices, findIndex, findIndices, isPrefixOf)
import Test.Hspec
import Text.Read (readMaybe)

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn delim str =
  let (first, remainder) = break (== delim) str
   in first : case remainder of
        [] -> []
        (_delim : rest) -> splitOn delim rest

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
      , ("2 / (2 + 3) * 4.33 - -6", 7.732)
      , ("12*-1", -12)
      , ("12* 123/-(-5 + 2)", 492)
      , ("89.94/(67.61)-(40.25+(11.49))+(98.41)/((75.76/(18.26))*(41.73)/(23.24)*95.62)+92.68+-8.56*77.11/(--61.46*-2.79*(87.35)-71.62*73.35-67.57+11.24*27.72)*7.38*10.7*20.13*78.89/((73.45/(57.52+76.61+98.37-53.13)))/((-11.98)-(4.44))", -573.500021609947)
      , ("(--54.51-(26.99)+33.64/(58.27))+--73.46/(24.61)*8.42+59.77/(79.21)-(--98.81+43.06)*--81.7-25.79+-86.76-55.77-55.89/(48.11)*74.69+-31.25/(4.23+((84.76*35.01+75.42)*66.01)*46.59*29.67-97.84-16.6/(69.75*-45.97))", -11791.882026648824)
      ]
      $ \(arg, expected) -> do
        it (show arg) $ do
          calc arg `shouldBe` expected

calc :: String -> Double
calc = solve

solve :: String -> Double
solve s =
  s
    & filter (/= ' ')
    & filter (/= '\n')
    & filter (/= '\t')
    & replaceAllHypens
    & solveParens

replace :: String -> String -> String -> String
replace _ _ "" = "" -- end of recursion, empty string
replace old new str@(x : xs)
  | old `isPrefixOf` str =
      let suffix = drop (length old) str
          suffix' = replace old new suffix -- recurse with shorter string
       in new ++ suffix'
  | otherwise = x : replace old new xs

replaceAllHypens :: String -> String
replaceAllHypens s =
  let s' =
        s
          -- Handling '-' which can mean subtract (binary op) or negate (unary op)
          -- Negation is encoded as '!', and subtraction as '+!', so there are no '-'
          & replace "---" "-"
          -- Double '-'
          & replace "(--" "(" -- TODO what a hack
          & replace "*--" "*"
          & replace "+--" "+"
          & replace "/--" "/"
          & replace "--" "+"
          -- Single '-'
          & replace "(-" "(!"
          & replace ")-" ")+!"
          & replace "*-" "*!"
          & replace "+-" "+!"
          & replace "/-" "/!"
      s'' = if s == s' then s' else replaceAllHypens s'
          -- Numbers follow by '-'
      s''' = s'' & replace "-" "+!"
   in s'''

solveNumber :: String -> Double
solveNumber "" = 0
solveNumber ('!' : s) = -solveNumber s
solveNumber s =
  let mn = readMaybe s
   in case mn of
        Just n -> n
        Nothing -> error $ "Could not parse number: \"" ++ s ++ "\""

solveAddition :: [Char] -> Double
solveAddition str =
  let parts = splitOn '+' str
      ns = map solveFactors parts
   in sum ns

solveParens :: [Char] -> Double
solveParens str =
  let parensIndices = findParens str
      parts = trisect str <$> parensIndices
      str' = solveTriple <$> parts
   in case str' of
        Nothing -> solveAddition str
        Just s -> solveParens s

findParens :: String -> Maybe (Int, Int)
findParens str =
  let mj = elemIndex ')' str
      is = elemIndices '(' str
      is' j = filter (< j) is
      i j = last $ is' j
   in case mj of
        Nothing -> Nothing
        Just j -> Just (i j, j)

trisect :: String -> (Int, Int) -> (String, String, String)
trisect str (i, j) =
  let a = take i str
      b = drop (i + 1) str & take (j - i - 1)
      c = drop (j + 1) str
   in (a, b, c)

solveTriple :: (String, String, String) -> String
solveTriple (a, b, c) = a ++ f b ++ c
 where
  f = solveAddition >>> show

getFactors :: String -> [String]
getFactors s =
  let i = elemIndex '*' s
      j = elemIndex '/' s
      k = min i j <|> i <|> j
   in case k of
        Nothing -> [s]
        Just idx ->
          let (a, b) = splitAt idx s
           in a : [head b] : getFactors (drop 1 b)

reduceFactors :: [String] -> Double
reduceFactors [x, o, y] =
  let x' = solveNumber x
      y' = solveNumber y
   in case o of
        "*" -> x' * y'
        "/" -> x' / y'
reduceFactors [y] = solveNumber y
reduceFactors (x : o : y : rest) =
  let a = reduceFactors [x, o, y]
      b = rest
   in reduceFactors (show a : b)

solveFactors :: String -> Double
solveFactors = getFactors >>> reduceFactors
