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

import Data.Foldable (for_)

import Data.Function ((&))
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
    -- Handling '-' which can mean subtract (binary op) or negate (unary op)
    -- Negation is encoded as '!', and subtraction as '+!', so there are no '-'
    -- Double '-'
    & replace "--" "+"
    -- Single '-'
    & replace "(-" "(!"
    & replace ")-" ")+!"
    & replace "*-" "*!"
    & replace "+-" "+!"
    & replace "/-" "/!"
    -- Numbers follow by '-'
    & replace "-" "+!"
    & solveParens

-- TODO review this function, AI wrote it
replace :: String -> String -> String -> String
replace _ _ [] = []
replace old new str@(x : xs)
  | old `isPrefixOf` str = new ++ replace old new (drop (length old) str)
  | otherwise = x : replace old new xs

solveNumber :: [Char] -> Double
solveNumber "" = 0
solveNumber ('!' : s) = -solveNumber s
solveNumber s =
  let mn = readMaybe s
   in case mn of
        Just n -> n
        Nothing -> error $ "Could not parse number: " ++ s

solveAddition :: [Char] -> Double
solveAddition str =
  let parts = splitOn '+' str
      ns = map solveMultiplication parts
   in sum ns

solveMultiplication :: [Char] -> Double
solveMultiplication str =
  let parts = splitOn '*' str
      ns = map solveDivision parts
   in product ns

solveDivision :: [Char] -> Double
solveDivision str =
  let parts = splitOn '/' str
      ns = map solveNumber parts
   in foldl1 (/) ns

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
solveTriple (a, b, c) = a ++ b' ++ c
 where
  b' = solveAddition b & show