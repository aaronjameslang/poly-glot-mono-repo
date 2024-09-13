-- -- Snail

-- A 4th kyu problem posed by codewars.com:
-- https://www.codewars.com/kata/521c2db8ddc89b9b7a0000c1

-- Snail Sort

-- Given an n x n array, return the array elements arranged from outermost elements to the middle element, traveling clockwise.

-- array = [[1,2,3],
--          [4,5,6],
--          [7,8,9]]
-- snail(array) #=> [1,2,3,6,9,8,7,4,5]

-- For better understanding, please follow the numbers of the next array consecutively:

-- array = [[1,2,3],
--          [8,9,4],
--          [7,6,5]]
-- snail(array) #=> [1,2,3,4,5,6,7,8,9]

-- NOTE: The idea is not sort the elements from the lowest value to the highest; the idea is to traverse the 2-d array in a clockwise snailshell pattern.

-- NOTE 2: The 0x0 (empty matrix) is represented as en empty array inside an array [[]].

module Snail where

import Test.Hspec

main :: IO ()
main = hspec $ do
    it "First example" $ do
        let array =
                [ [1, 2, 3]
                , [4, 5, 6]
                , [7, 8, 9]
                ]
            expected = [1, 2, 3, 6, 9, 8, 7, 4, 5]
        snail array `shouldBe` expected

    it "Second example" $ do
        let array =
                [ [1, 2, 3]
                , [8, 9, 4]
                , [7, 6, 5]
                ]
            expected = [1, 2, 3, 4, 5, 6, 7, 8, 9]
        snail array `shouldBe` expected

    it "Size zero" $ do
        let array = [[]]
            expected = []
        snail array `shouldBe` expected

    it "Size two" $ do
        let array =
                [ [1, 2]
                , [4, 3]
                ]
            expected = [1, 2, 3, 4]
        snail array `shouldBe` expected

-- This seems easier than EquivalentDice, depsite being a higher kyu

snail :: [[Int]] -> [Int]
snail [] = []
snail [[]] = []
snail matrix =
    let size = length matrix
        coords = getLayers size 0
        deref (Coord (x, y)) = matrix !! y !! x
     in map deref coords

newtype Coord = Coord (Int, Int)
    deriving (Eq, Ord, Show)

getTopRow :: Int -> Int -> [Coord]
getTopRow size layer =
    let layer' = size - layer - 1
        x0 = layer
        xn = layer' - 1
        y = layer
     in [Coord (x, y) | x <- [x0 .. xn]]

getRightColumn :: Int -> Int -> [Coord]
getRightColumn size layer =
    let layer' = size - layer - 1
        x = layer'
        y0 = layer
        yn = layer' - 1
     in [Coord (x, y) | y <- [y0 .. yn]]

getBottomRow :: Int -> Int -> [Coord]
getBottomRow size layer =
    let layer' = size - layer - 1
        x0 = layer'
        xn = layer + 1
        y = layer'
     in [Coord (x, y) | x <- [x0, x0 - 1 .. xn]]

getLeftColumn :: Int -> Int -> [Coord]
getLeftColumn size layer =
    let layer' = size - layer - 1
        x = layer
        y0 = layer'
        yn = layer + 1
     in [Coord (x, y) | y <- [y0, y0 - 1 .. yn]]

getLayer :: Int -> Int -> [Coord]
getLayer size layer =
    getTopRow size layer
        ++ getRightColumn size layer
        ++ getBottomRow size layer
        ++ getLeftColumn size layer

countLayers :: Int -> Int
countLayers size = (size + 1) `div` 2

getLayers :: Int -> Int -> [Coord]
getLayers size layer
    | layer < countLayers size - 1 = getLayer size layer ++ getLayers size (layer + 1)
    | even size =
        [ Coord (layer, layer)
        , Coord (layer + 1, layer)
        , Coord (layer + 1, layer + 1)
        , Coord (layer, layer + 1)
        ]
    | otherwise = [Coord (layer, layer)]

-- \| 2 * layer == size + 1 = [Coord (layer, layer)]
-- \| otherwise = getLayer size layer ++ getLayers size (layer + 1)
