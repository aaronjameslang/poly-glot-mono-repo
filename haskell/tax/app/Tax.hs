module Tax where

-- Tax

-- Band                 Taxable income          Tax rate
-- Personal Allowance 	Up to £12,570           0%
-- Basic rate           £12,571 to £50,270      20%
-- Higher rate          £50,271 to £125,140   	40%
-- Additional rate 	    over £125,140       	  45%

-- https://money.stackexchange.com/questions/150677/uk-taxation-should-i-be-able-to-calculate-my-paye-tax-for-the-year-exactly
-- https://community.hmrc.gov.uk/customerforums/sa/9f955083-87a9-ee11-a81c-002248004b84

-- TODO Use integer pence, and round results

data Band = Band
  { bot :: Double
  , top :: Double
  , rate :: Double
  , name :: String
  }

bands :: [Band]
bands =
  [ Band{bot = 0, top = 12_750, rate = 0.00, name = "personal"}
  , Band{bot = 12_750, top = 50_720, rate = 0.20, name = "basic"}
  , Band{bot = 50_720, top = 125_140, rate = 0.40, name = "higher"}
  , Band{bot = 125_140, top = 1 / 0, rate = 0.45, name = "additional"}
  ]

calculateTaxInBand :: Double -> Band -> Double
calculateTaxInBand income (Band{top, bot, rate})
  | income <= bot = 0 -- empty band
  | income <= top = (income - bot) * rate
  | income >= top = (top - bot) * rate
  | otherwise = error "calculateTaxInBand: impossible case"

calculateTaxTotal :: Double -> Double
calculateTaxTotal income = sum $ map (calculateTaxInBand income) bands

calculateTax :: Double -> Double
calculateTax = calculateTaxTotal