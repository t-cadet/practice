import Test.HUnit
import Data.Maybe

import Lib

main = runTestTT tests

tests = test $ 
       (testCuanu cuanuNaive) 
    ++ (testCuanu countUniqueAndNonUnique)
    ++ testDigits
    ++ testToLuhnForm
    ++ testIsLuhn
    ++ testSirenFromInt
    ++ testSirenTextToList

testCuanu cuanu = [ 
      "cuanu" ~: "" ~: (0,0) ~=? (cuanu ([]::[Integer]))
    , "cuanu" ~: "" ~: (1,0) ~=? (cuanu [0])
    , "cuanu" ~: "" ~: (0,1) ~=? (cuanu [0, 0])
    , "cuanu" ~: "" ~: (1,1) ~=? (cuanu [0, 1, 1])
    , "cuanu" ~: "" ~: (0,6) ~=? (cuanu ([0..5]++[0..5]))
    , "cuanu" ~: "" ~: (3,2) ~=? (cuanu [0, 1, 1, 2, 3, 4, 4])
    , "cuanu" ~: "" ~: (1,5) ~=? (cuanu (6:(take 70000 $ cycle [0, 1, 1, 2, 3, 4, 4])))
    ]

testDigits = [
      "digits" ~: "" ~: [0] ~=? (digits 0)
    , "digits" ~: "" ~: [1] ~=? (digits 1)
    , "digits" ~: "" ~: [1, 0] ~=? (digits 10)
    , "digits" ~: "" ~: [7, 2, 3, 8, 7, 0] ~=? (digits 723870)
    ]

testToLuhnForm = [
      "toLuhnForm" ~: "" ~: [0] ~=? (toLuhnForm 0)
    , "toLuhnForm" ~: "" ~: [1] ~=? (toLuhnForm 1)
    , "toLuhnForm" ~: "" ~: [2, 0] ~=? (toLuhnForm 10)
    , "toLuhnForm" ~: "" ~: [5, 2, 6, 8, 5, 0] ~=? (toLuhnForm 723870)
    , "toLuhnForm" ~: "" ~: [9, 5, 2, 8, 8, 5, 0, 7, 6] ~=? (toLuhnForm 972487086)
    , "toLuhnForm" ~: "" ~: [9, 4, 7, 8, 8, 5, 0, 7, 6] ~=? (toLuhnForm 927487086)
    ]

testIsLuhn = [
      "isLuhn" ~: "" ~: True ~=? (isLuhn 0)
    , "isLuhn" ~: "" ~: False ~=? (isLuhn 1)
    , "isLuhn" ~: "" ~: False ~=? (isLuhn 10)
    , "isLuhn" ~: "" ~: False ~=? (isLuhn 723870)
    , "isLuhn" ~: "" ~: True ~=? (isLuhn 972487086)
    , "isLuhn" ~: "" ~: False ~=? (isLuhn 927487086)
    , "isLuhn" ~: "" ~: True ~=? (isLuhn 97248708600)
    ]

testSirenFromInt = [
      "sirenFromInt" ~: "" ~: True ~=? isJust (sirenFromInt 0)
    , "sirenFromInt" ~: "" ~: Nothing ~=? (sirenFromInt (-1))
    , "sirenFromInt" ~: "" ~: Nothing ~=? (sirenFromInt (10^9))
    , "sirenFromInt" ~: "" ~: Nothing ~=? (sirenFromInt 1)
    , "sirenFromInt" ~: "" ~: Nothing ~=? (sirenFromInt 10)
    , "sirenFromInt" ~: "" ~: Nothing ~=? (sirenFromInt 723870)
    , "sirenFromInt" ~: "" ~: True ~=? isJust (sirenFromInt 972487086)
    , "sirenFromInt" ~: "" ~: Nothing ~=? (sirenFromInt 927487086)
    , "sirenFromInt" ~: "" ~: Nothing ~=? (sirenFromInt 97248708600)    
    ]

testSirenTextToList = 
    let text = "000325175\n005420021\n005420120\n005420120\n005420120\n005450093\n005450093\n005450119\n005450119\n005480546\ngarbage\n0054805467\n-1\ndjhdlksq\n\n1dsfn\n723870"
        in ["sirenTextToList" ~: "" ~: 10 ~=? length (sirenTextToList text)]