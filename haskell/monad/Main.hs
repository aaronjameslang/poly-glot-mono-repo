import qualified GuessGame

-- Use `cabal run` to run all the tests

main :: IO ()
main = do
    GuessGame.main
    putStrLn "Done"
