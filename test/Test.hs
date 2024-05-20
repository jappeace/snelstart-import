module Main where

import qualified Data.Vector as Vector
import SnelstartN26Import
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "List comparison (different length)" $ do
        result <- SnelstartN26Import.readN26 "n26.csv"
        result
          @?= Right
            ( Vector.fromList
                [ N26
                    { date = read "2024-05-03 00:00:00 UTC",
                      payee = "ASR SCHADEVERZEKERING",
                      accountNumber = "NL59ABNA0240576861",
                      transactionType = DirectDebit,
                      paymentReference = "55104021 Premie periode 01-05-2024 - 31-05-2024 Voordeelpakket Bedrijven",
                      amountEur = -10.08,
                      amountForegin = Nothing,
                      typeForeign = "",
                      exchangeRate = ""
                    },
                  N26
                    { date = read "2024-05-04 00:00:00 UTC",
                      payee = "CHATGPT SUBSCRIPTION",
                      accountNumber = "",
                      transactionType = MastercardPayment,
                      paymentReference = "",
                      amountEur = -22.62,
                      amountForegin = Just (-24.2),
                      typeForeign = "USD",
                      exchangeRate = "0.9347107438"
                    }
                ]
            ),
      testCase "run main" $ do
        SnelstartN26Import.main
    ]
