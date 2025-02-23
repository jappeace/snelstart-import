module Main where

import qualified Data.Vector as Vector
import SnelstartImport.N26
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
            result <- readN26 "n26-v2.csv"
            result
                @?= Right
                    ( Vector.fromList
                        [ N26
                            { date = read "2024-07-10 00:00:00 UTC"
                            , valueDate = read "2024-07-10 00:00:00 UTC"
                            , payee = "MY COMPUTER"
                            , accountNumber = ""
                            , transactionType = MastercardPayment
                            , paymentReference = ""
                            , accountName = "Main Account"
                            , amountEur = -206.27
                            , amountForegin = Just 222.85
                            , typeForeign = "USD"
                            , exchangeRate = "0.9256001795"
                            }
                        , N26
                            { date = read "2024-07-15 00:00:00 UTC"
                            , valueDate = read "2024-07-15 00:00:00 UTC"
                            , payee = "JT Klooster"
                            , accountNumber = "NL74INGB0798449241"
                            , transactionType = OutgoingTransfer
                            , paymentReference = ""
                            , accountName = "Main Account"
                            , amountEur = -800.0
                            , amountForegin = Nothing
                            , typeForeign = ""
                            , exchangeRate = ""
                            }
                        , N26
                            { date = read "2024-07-17 00:00:00 UTC"
                            , valueDate = read "2024-07-17 00:00:00 UTC"
                            , payee = "Oeverture belasting advies b.v."
                            , accountNumber = "NL90RABO0136021050"
                            , transactionType = OutgoingTransfer
                            , paymentReference = ""
                            , accountName = "Main Account"
                            , amountEur = -1185.8
                            , amountForegin = Nothing
                            , typeForeign = ""
                            , exchangeRate = ""
                            }
                        , N26
                            { date = read "2024-07-17 00:00:00 UTC"
                            , valueDate = read "2024-07-17 00:00:00 UTC"
                            , payee = "Administratiekantoor De Wilde via Stichting Mollie Payments"
                            , accountNumber = "NL51DEUT0265262461"
                            , transactionType = OutgoingTransfer
                            , paymentReference = "c37f91dbcae0a3e00b91496426f3c954 8152518244016285 20240754 Administratiekantoor De Wilde"
                            , accountName = "Main Account"
                            , amountEur = -656.43
                            , amountForegin = Nothing
                            , typeForeign = ""
                            , exchangeRate = ""
                            }
                        ]
                    )
        ]
