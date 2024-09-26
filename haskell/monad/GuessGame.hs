-- -- Guessing Game (with Monads)

-- A 6th kyu problem posed by codewars.com:
-- https://www.codewars.com/kata/5c607bb95a40bc7b4b9ecc27

-- Your task is to guess a number between 1 and 100 using only 7 tests.

-- You are given a function greaterThan :: Monad m => Int -> m Bool.

module GuessGame (guess, main) where

import Control.Monad.Reader
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main =
    let greaterThan n = asks (> n)
        runGame = runReader (guess greaterThan)
     in hspec $ do
            it "can guess 42" $ property $ do
                runGame 42 `shouldBe` 42
            it "can guess correctly" $ property $ do
                forAll (choose (1, 100)) $ \n ->
                    runGame n `shouldBe` n

-- True, the answer is greater than the current guess
-- False, the answer is less than our equal to the current guess
type CheckGuess m = Int -> m Bool

data Bounds = Bounds {lower :: Int, upper :: Int}

guess :: (Monad m) => CheckGuess m -> m Int
guess checkGuess =
    let initialBounds = Bounds 1 100
     in calcAnswer checkGuess initialBounds

-- Well, this passes the tests
-- Still don't feel like I really grok monads
calcAnswer :: (Monad m) => CheckGuess m -> Bounds -> m Int
calcAnswer checkGuess bounds = do
    let nextGuess = calcNextGuess bounds
    guessHigher <- checkGuess nextGuess
    let nextBounds = calcNextBounds bounds nextGuess guessHigher
    let answer = hasAnswer nextBounds
    case answer of
        Just n -> return n
        Nothing -> calcAnswer checkGuess nextBounds

-- calcAnswer checkGuess bounds =
--     let nextGuess = calcNextGuess bounds
--     in checkGuess nextGuess >>= \guessHigher ->
--        let nextBounds = calcNextBounds bounds nextGuess guessHigher
--            answer = hasAnswer nextBounds
--        in case answer of
--            Just n  -> return n
--            Nothing -> calcAnswer checkGuess nextBounds

calcNextGuess :: Bounds -> Int
calcNextGuess (Bounds l u) = (l + u) `div` 2

calcNextBounds :: Bounds -> Int -> Bool -> Bounds
calcNextBounds (Bounds{lower, upper}) guess higher =
    if higher
        -- True, the answer is greater than the current guess
        then Bounds (guess + 1) upper
        -- False, the answer is less than our equal to the current guess
        else Bounds lower guess

hasAnswer :: Bounds -> Maybe Int
hasAnswer (Bounds l u) =
    if l == u
        then Just l
        else Nothing
