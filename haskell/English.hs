#! /usr/bin/env runhaskell

-- Wherein we abuse haskell's lack of parentheses to
-- try to make it understand simple English statements

import Prelude hiding (even, not, odd)

assert :: Bool -> IO ()
assert actual = if not actual then error "Assertion failed" else putStr ""

odd :: Int -> Bool
odd n = n `mod` 2 == 1

even :: Int -> Bool
even n = n `mod` 2 == 0

is :: a -> (a -> Bool) -> Bool
is a f = f a

-- is = flip ($)
-- is = flip id

class Negatable a where
    not :: a -> a

instance Negatable Bool where
    not True = False
    not False = True

instance Negatable (a -> Bool) where
    -- not f = \x -> not (f x)
    -- not f x = not (f x)
    not f = not . f

submain0 :: IO ()
submain0 = do
    assert $ True
    assert $ not False
    assert $ odd 1
    assert $ even 2
    assert $ 1 `is` odd
    assert $ 2 `is` even
    assert $ 1 `is` not even
    assert $ 2 `is` not odd

data Language = French | English | German deriving (Eq)

-- data Person = Person Language
newtype Person = Person Language

pierre = Person French
john = Person English
hans = Person German

speaks :: Person -> Language -> Bool
speaks (Person l1) l2 = l1 == l2

-- speaks (Person language) = (==) language

speak :: Language -> Person -> Bool
speak = flip speaks

does :: a -> (a -> Bool) -> Bool
does a f = f a

submain1 :: IO ()
submain1 = do
    assert $ pierre `speaks` French
    assert $ hans `speaks` German
    assert $ pierre `does` speak French
    assert $ pierre `does` not (speak German) -- TODO Why are parens needed? I don't think I understand the precedence rules

main :: IO ()
main = do
    submain0
    submain1
    putStrLn "All tests passed"