#! /usr/bin/env runhaskell

-- An experiment in polymorphism and operator overloading
-- https://en.wikipedia.org/wiki/£sd Lsd

shouldBe :: String -> String -> IO ()
shouldBe actual expected =
    if actual == expected
        then putStrLn (actual ++ " is " ++ expected)
        else error $ actual ++ " is not " ++ expected

newtype OldMoney = OldMoney Int

-- pence x = OldMoney x
pence = OldMoney
shillings x = pence (12 * x)
pounds x = shillings (20 * x)

instance Eq OldMoney where
    OldMoney a == OldMoney b = a == b

instance Num OldMoney where
    (OldMoney a) + (OldMoney b) = OldMoney (a + b)
    (OldMoney a) - (OldMoney b) = OldMoney (a - b)

instance Show OldMoney where
    show (OldMoney a)
        | a < 12 = show a ++ "d"
        | a < 240 = show (a `div` 12) ++ "s " ++ show (OldMoney (a `mod` 12))
        | otherwise = show (a `div` 240) ++ "L " ++ show (OldMoney (a `mod` 240))

main :: IO ()
main = do
    show (pence 0) `shouldBe` "0d"
    show (pence 1) `shouldBe` "1d"
    show (pence 8) `shouldBe` "8d"
    show (pence 8 + pence 1) `shouldBe` "9d"
    show (pence 12) `shouldBe` "1s 0d"
    show (pence 14) `shouldBe` "1s 2d"
    show (shillings 5 + pence 18) `shouldBe` "6s 6d"
    show (pounds 2 + shillings 50 + pence 3) `shouldBe` "4L 10s 3d"
    show (pounds 2 + pence (-10)) `shouldBe` "1L 19s 2d"
    show (pounds 2 - pence 10) `shouldBe` "1L 19s 2d"
    putStrLn "All tests passed"
