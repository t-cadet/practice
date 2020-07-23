import Test.HUnit

import Lib

main = runTestTT tests

tests = test $ (testCuanu cuanuNaive) ++ (testCuanu countUniqueAndNonUnique)

testCuanu cuanu = [ 
      "" ~: "" ~: (0,0) ~=? (cuanu ([]::[Integer]))
    , "" ~: "" ~: (1,0) ~=? (cuanu [0])
    , "" ~: "" ~: (0,1) ~=? (cuanu [0, 0])
    , "" ~: "" ~: (1,1) ~=? (cuanu [0, 1, 1])
    , "" ~: "" ~: (0,6) ~=? (cuanu ([0..5]++[0..5]))
    , "" ~: "" ~: (3,2) ~=? (cuanu [0, 1, 1, 2, 3, 4, 4])
    , "" ~: "" ~: (1,5) ~=? (cuanu (6:(take 70000 $ cycle [0, 1, 1, 2, 3, 4, 4])))
    ]
