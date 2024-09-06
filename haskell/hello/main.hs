main = do
  putStrLn "Hello, World!"
  putStrLn "Who are you?"
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"