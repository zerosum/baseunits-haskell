module MoneyTest where

import Currency (Alpha (..), Currency, fromAlpha)
import Money
import Test.Tasty
import Test.Tasty.HUnit

test_case2 :: TestTree
test_case2 =
  testGroup
    "CreationFromDouble"
    [ testCase "15.0 `USD` from Double" $
        adjustByDouble 15.0 USD @?= d15,
      testCase "2.51 `USD` from Double" $
        adjustByDouble 2.51 USD @?= d2_51,
      testCase "50.1 `JPY` from Double" $
        adjustByDouble 50.1 JPY @?= y50,
      testCase "100 `JPY` from Double" $
        adjustByDouble 100 JPY @?= y100
    ]
  where
    d15 = adjustBy (read "15.0") USD
    d2_51 = adjustBy (read "2.51") USD
    y50 = adjustBy (read "50") JPY
    y100 = adjustBy (read "100") JPY

test_case3 :: TestTree
test_case3 =
  testGroup
    "Yen"
    [ testCase "50 jpy + 30 jpy == 80 jpy" $
        y50 `plus` y30 @?= y80,
      testCase "50 jpy * 1.6 == 80 jpy" $
        y50 `times` 1.6 @?= y80
    ]
  where
    y30 = adjustByDouble 30 JPY
    y50 = adjustBy (read "50") JPY
    y80 = adjustBy (read "80") JPY

test_case4 :: TestTree
test_case4 =
  testGroup
    "money"
    [ testCase "amount" $
        breachEncapsulationOfAmount d69_99 @?= read "69.99",
      testCase "currency" $
        breachEncapsulationOfCurrency d69_99 @?= fromAlpha USD
    --   testCase "exception if invalid scale" $
    --     breachEncapsulationOfAmount (money (read "69.999") USD) @?= read "69.99"
    ]
  where
    d69_99 = money (read "69.99") USD
