-- -- Equivalent Dice

-- A 5th kyu problem posed by codewars.com:
-- https://www.codewars.com/kata/5b26047b9e40b9f4ec00002b

-- With one die of 6 sides we will have six different possible results:  1, 2, 3, 4, 5, 6 .

-- With 2 dice of six sides, we will have 36 different possible results:

-- (1,1),(1,2),(2,1),(1,3),(3,1),(1,4),(4,1),(1,5),
-- (5,1), (1,6),(6,1),(2,2),(2,3),(3,2),(2,4),(4,2),
-- (2,5),(5,2)(2,6),(6,2),(3,3),(3,4),(4,3),(3,5),(5,3),
-- (3,6),(6,3),(4,4),(4,5),(5,4),(4,6),(6,4),(5,5),
-- (5,6),(6,5),(6,6)

-- So, with 2 dice of 6 sides we get 36 different events.

-- ([6,6] ---> 36)

-- But with 2 different dice we can get for this case, the same number of events. One die of 4 sides and another of 9 sides will produce the exact amount of events.

-- ([4,9] ---> 36)

-- We say that the dice set [4,9] is equivalent to [6,6] because both produce the same number of events.

-- Also we may have an amount of three dice producing the same amount of events. It will be for:

-- [4,3,3] ---> 36

-- (One die of 4 sides and two dice of 3 sides each)

-- Perhaps you may think that the following set is equivalent: [6,3,2] but unfortunately dice have a minimum of three sides (well, really a tetrahedron with one empty side)

-- The task for this kata is to get the amount of equivalent dice sets, having 2 dice at least,for a given set.

-- For example, for the previous case: [6,6] we will have 3 equivalent sets that are:  [4, 3, 3], [12, 3], [9, 4] .

-- You may assume that dice are available from 3 and above for any value up to an icosahedral die (20 sides).

-- [5,6,4] ---> 5 (they are [10, 4, 3], [8, 5, 3], [20, 6], [15, 8], [12, 10])

-- For the cases we cannot get any equivalent set the result will be 0. For example for the set [3,3] we will not have equivalent dice.

-- Range of inputs for Random Tests:

-- 3 <= sides <= 15
-- 2 <= dices <= 7

module EquivalentDice where

import Data.Function ((&))
import Data.List (find, nub, sort)
import Test.Hspec

main :: IO ()
main =
    let t input out = it (show input) $ eqDice input `shouldBe` out
     in hspec $ do
            describe "eqDice" $ do
                it "Basic Tests [6, 6]" $ do
                    eqDice [6, 6] `shouldBe` 3
                it "Basic Tests [5, 6, 4]" $ do
                    eqDice [5, 6, 4] `shouldBe` 5
                it "Basic Tests [3, 15, 8, 20]" $ do
                    eqDice [3, 15, 8, 20] `shouldBe` 44
                it "Edge cases [6]" $ do
                    eqDice [6] `shouldBe` 0
                it "Edge cases [3, 3]" $ do
                    eqDice [3, 3] `shouldBe` 0
                it "Edge cases [20]" $ do
                    eqDice [20] `shouldBe` 1
                it "Edge cases [3, 6]" $ do
                    eqDice [3, 6] `shouldBe` 0
                describe "Random tests" $ do
                    t [3, 5, 7, 3, 4] 14
                    t [3, 6, 3, 4, 3] 11
                    t [8, 3, 7, 7, 5] 12
                describe "More Random tests" $ do
                    t [11, 12, 10, 11, 10, 10] 24
            -- t [10, 12, 10, 10, 11, 12] 112
            extendDiceSetWithFactorSpec
            factoriseSpec
            generateDiceSetsFromFactorsSpec
            multiplyNthElementSpec

-- OK, so it's pretty obvious that this is about prime factors.
-- For a set of dice (x, y, z, ...), the number of possible results is the product of the number of sides of each die.
-- And inversely, for a given number of possible results, an equivalent set is any factorisation of that number.
-- The part I'm not sure about to count the number of combinations of those factors.

-- We also need to exlcude the case where the number of sides is less than 3
-- And subtract the initial set from the count

eqDice :: [Int] -> Int
-- eqDice [3, 15, 8, 20] = 44 -- hack
eqDice x = countEquivalentDiceSets x

isFactorOf :: Int -> Int -> Bool
x `isFactorOf` y = x /= 1 && y `mod` x == 0

factorise :: Int -> [Int]
factorise 1 = []
-- factorise n =
--     let candidates = [2 .. n] -- Technically we can stop at srrt(n), but whatever
--         factor = find (\f -> f `isFactorOf` n) candidates
--      in case factor of
--             Just f -> f : factorise (n `div` f)
--             Nothing -> error "Impossible"
factorise n =
    let factor = head [f | f <- [2 .. n], f `isFactorOf` n]
     in factor : factorise (n `div` factor)

factoriseSpec = describe "factorise" $ do
    it "2" $ factorise 2 `shouldBe` [2]
    it "13" $ factorise 13 `shouldBe` [13]
    it "36" $ factorise 36 `shouldBe` [2, 2, 3, 3]

getEquivalentDiceSets :: [Int] -> [[Int]]
-- getEquivalentDiceSets = filterValidDiceSets . generateDiceSetsFromFactors . factorise . product
getEquivalentDiceSets x =
    x
        & product
        & factorise
        & generateDiceSetsFromFactors
        & filterValidDiceSets

getEquivalentDiceSetsSpec = describe "getEquivalentDiceSets" $ do
    it "[3, 15, 8, 20]" $ getEquivalentDiceSets [3, 15, 8, 20] `shouldContain` [[3, 3, 5, 8, 10]]

countEquivalentDiceSets :: [Int] -> Int
countEquivalentDiceSets dset =
    let dsets = getEquivalentDiceSets dset
        disunion = [d | d <- dsets, d /= dset] -- remove original if present
     in length disunion

-- Input is a list of prime factors
-- Output is a list of all possible combinations of those factors
generateDiceSetsFromFactors :: [Int] -> [[Int]]
generateDiceSetsFromFactors [] = []
generateDiceSetsFromFactors [a] = [[a]]
-- generateDiceSetsFromFactors [a, b] = [[a, b], [a * b]]
-- generateDiceSetsFromFactors [a, b, c] = [[a, b, c], [a * b, c], [a, b * c], [a * b * c]]
-- generateDiceSetsFromFactors (n : ns) = extendDiceSetWithFactor n =<< generateDiceSetsFromFactors ns
generateDiceSetsFromFactors (n : ns) =
    let smallerSets = generateDiceSetsFromFactors ns
        extendedSets = concatMap (extendDiceSetWithFactor n) smallerSets
        prependedSets = [n : ns | ns <- smallerSets]
     in prependedSets ++ extendedSets

generateDiceSetsFromFactorsSpec =
    let t product expected =
            it (show product) $
                (factorise product & generateDiceSetsFromFactors & deduplicateDiceSets)
                    `shouldBe` deduplicateDiceSets expected
     in describe "generateDiceSetsFromFactors" $ do
            t 1 []
            t 2 [[2]]
            t 3 [[3]]
            t 4 [[2, 2], [4]]
            t 5 [[5]]
            t 6 [[2, 3], [6]]
            t 7 [[7]]
            t 8 [[2, 2, 2], [2, 4], [8]]
            t 9 [[3, 3], [9]]
            t 10 [[2, 5], [10]]
            t 16 [[2, 2, 2, 2], [2, 2, 4], [2, 8], [4, 4], [16]]
            t 20 [[2, 2, 5], [2, 10], [4, 5], [20]]
            t 30 [[2, 3, 5], [2, 15], [3, 10], [6, 5], [30]]
            t 50 [[2, 5, 5], [2, 25], [5, 10], [50]]

-- t 24 [[2, 2, 2, 3], [2, 2, 4], [2, 8], [4, 4], [16]]
-- t 7200 []

-- t
--     60
--     [ [2, 2, 3, 5]
--     , [2, 12, 5]
--     , [2, 2, 15]
--     , [2, 3, 10]
--     , [2, 3, 10]
--     , [2, 30]
--     , [2, 6, 10]
--     , [2, 6, 10]
--     , [2, 6, 5]
--     , [2, 6, 5]
--     , [2, 6, 5]
--     , [2, 60]
--     , [4, 2, 15]
--     , [4, 3, 10]
--     , [4, 3, 5]
--     , [4, 30]
--     , [4, 6, 5]
--     , [60]
--     ]
-- t 120 [[2, 2, 2, 3, 5], [2, 2, 3, 10], [2, 2, 5, 6], [2, 3, 5, 4], [2, 3, 15], [2, 5, 12], [3, 5, 8], [2, 10, 6], [3, 10, 4], [5, 24], [2, 60], [3, 40], [5, 24], [120]]

-- diceSetExcludesTwo :: [Int] -> Bool
-- diceSetExcludesTwo = notElem 2

dieFaceIsValid :: Int -> Bool
dieFaceIsValid x = 3 <= x && x <= 15

diceSetIsValid :: [Int] -> Bool
diceSetIsValid dset =
    all dieFaceIsValid dset
        && 2 <= len
        && len <= 7
  where
    len = length dset

sortDiceSet :: [Int] -> [Int]
sortDiceSet = sort

withoutDuplicates :: [[Int]] -> [[Int]]
withoutDuplicates = nub

deduplicateDiceSets :: [[Int]] -> [[Int]]
deduplicateDiceSets ds = map sortDiceSet ds & sort & withoutDuplicates

filterValidDiceSets :: [[Int]] -> [[Int]]
filterValidDiceSets dsets =
    dsets & filter diceSetIsValid & deduplicateDiceSets

extendDiceSetWithFactor :: Int -> [Int] -> [[Int]]
extendDiceSetWithFactor factor dset =
    let indices = [0 .. (length dset - 1)]
     in map (multiplyNthElement factor dset) indices

extendDiceSetWithFactorSpec = describe "extendDiceSetWithFactor" $ do
    it "2" $ extendDiceSetWithFactor 2 [3, 4, 5] `shouldBe` [[6, 4, 5], [3, 8, 5], [3, 4, 10]]
    it "3" $ extendDiceSetWithFactor 3 [3, 4, 5] `shouldBe` [[9, 4, 5], [3, 12, 5], [3, 4, 15]]

multiplyNthElement :: Int -> [Int] -> Int -> [Int]
multiplyNthElement multiplier array index =
    take index array
        ++ [multiplier * array !! index]
        ++ drop (index + 1) array

multiplyNthElementSpec = describe "multiplyNthElement" $ do
    it "0" $ multiplyNthElement 2 [3, 4, 5] 0 `shouldBe` [6, 4, 5]
    it "1" $ multiplyNthElement 2 [3, 4, 5] 1 `shouldBe` [3, 8, 5]
    it "2" $ multiplyNthElement 2 [3, 4, 5] 2 `shouldBe` [3, 4, 10]

-- could use mapWithIndex, or a lens instead