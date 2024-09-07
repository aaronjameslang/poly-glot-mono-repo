-- -- Buying a Car

-- A 6th kyu problem posed by codewars.com:
-- https://www.codewars.com/kata/554a44516729e4d80b000012

-- A man has a rather old car being worth $2000. He saw a secondhand car being
-- worth $8000. He wants to keep his old car until he can buy the secondhand one.

-- He thinks he can save $1000 each month but the prices of his old car and of the
-- new one decrease of 1.5 percent per month. Furthermore this percent of loss
-- increases of 0.5 percent at the end of every two months. Our man finds it
-- difficult to make all these calculations.

-- How many months will it take him to save up enough money to buy the car he
-- wants, and how much money will he have left over?

import Test.Hspec
import Text.ParserCombinators.ReadP (get)

main = hspec $ do
    describe "nbMonths" $ do
        it "1st series" $ do
            nbMonths 2000 8000 1000 1.5 `shouldBe` [6, 766]
            nbMonths 12000 8000 1000 1.5 `shouldBe` [0, 4000]

nbMonths :: Integer -> Integer -> Integer -> Double -> [Integer]
nbMonths startPriceOld startPriceNew savingPerMonth percentLossByMonth =
    let initialState = getInitialState startPriceOld startPriceNew percentLossByMonth
        finalState = getFinalState savingPerMonth initialState
     in [month finalState, round $ getPriceDifference finalState]

data State = State
    { month :: Integer
    , savings :: Integer
    , priceOld :: Double
    , priceNew :: Double
    , percentLoss :: Double
    }
    deriving (Show)

getInitialState :: Integer -> Integer -> Double -> State
getInitialState startPriceOld startPriceNew percentLossByMonth =
    State
        { month = 0
        , savings = 0
        , priceOld = fromInteger startPriceOld
        , priceNew = fromInteger startPriceNew
        , percentLoss = percentLossByMonth
        }

getNextState :: Integer -> State -> State
getNextState savingPerMonth state =
    state
        { month = month state + 1
        , savings = savings state + savingPerMonth
        , priceOld = priceOld state * (1 - percentLoss state / 100)
        , priceNew = priceNew state * (1 - percentLoss state / 100)
        , percentLoss =
            if even $ month state
                then percentLoss state + 0.5
                else percentLoss state
        }

getFinalState :: Integer -> State -> State
getFinalState savingPerMonth state =
    if getPriceDifference state >= 0
        then state
        else getFinalState savingPerMonth $ getNextState savingPerMonth state

getPriceDifference :: State -> Double
getPriceDifference state =
    (fromInteger (savings state) :: Double)
        + priceOld state
        - priceNew state