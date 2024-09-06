import Control.Exception (evaluate)
import Tax (
  Band (name),
  bands,
  calculateTaxInBand,
  calculateTaxTotal,
 )
import Test.Hspec

-- TODO Test pence, negative, zero, and rounding
-- TODO Measure coverage

testCasesTaxInBand =
  [ (0, head bands, 0)
  , (1_000, head bands, 0)
  , (1_000_000, head bands, 0)
  , (0, bands !! 1, 0)
  , (1_000, bands !! 1, 0)
  , (12_755, bands !! 1, 1)
  , (12_850, bands !! 1, 20)
  , (60_000, bands !! 1, 7594)
  , (80_000, bands !! 1, 7594)
  ]

testCasesTaxTotal =
  [ (0, 0)
  , (1, 0)
  , (1_000, 0)
  , (10_000, 0)
  , (12_750, 0)
  , (12_755, 1)
  , (12_850, 20)
  , (50_710, 7_592)
  , (50_720, 7_594)
  , (50_725, 7_596)
  , (125_100, 37_346)
  , (125_140, 37_362)
  , (125_240, 37_407)
  , (200_000, 71_049)
  ]

main :: IO ()
main = hspec $ do
  describe "calculateTaxInBand" $ do
    mapM_ specifyTaxInBand testCasesTaxInBand
  describe "calculateTaxTotal" $ do
    mapM_ specifyTaxTotal testCasesTaxTotal

specifyTaxTotal :: (Double, Double) -> Spec
specifyTaxTotal (income, expected) =
  it ("calculateTaxTotal " ++ show income ++ " should be " ++ show expected) $ do
    calculateTaxTotal income `shouldBe` expected

specifyTaxInBand :: (Double, Band, Double) -> Spec
specifyTaxInBand (income, band, expected) =
  it ("calculateTaxInBand " ++ show income ++ " " ++ name band ++ " should be " ++ show expected) $ do
    calculateTaxInBand income band `shouldBe` expected
