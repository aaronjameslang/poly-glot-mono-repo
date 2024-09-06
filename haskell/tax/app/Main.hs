import System.Environment (getArgs)
import Tax (calculateTaxTotal)

main :: IO ()
main = do
  args <- getArgs
  let income = read (head args)
  let tax = calculateTaxTotal income
  putStrLn $ "Tax: " ++ show tax
  putStrLn $ "Income after tax: " ++ show (income - tax)
