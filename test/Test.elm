import Data.Decimal exposing (..)
import Maybe exposing (Maybe)
import ElmTest exposing (..)
import Graphics.Element exposing (show, flow, down, Element, leftAligned)

liftMaybe : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe f a b =
    case (a, b) of
        (Just va, Just vb) -> Just (f va vb)
        _ -> Nothing
dtoString = Data.Decimal.toString
dnegate = Data.Decimal.negate
dcompare : Maybe Decimal -> Maybe Decimal -> Maybe Order
dcompare = liftMaybe Data.Decimal.compare
dtruncate = Data.Decimal.truncate
dround = Data.Decimal.round
uFromString = Data.Decimal.unsafeFromString

addTests : Test
addTests =
    let one = fromInt 1
        two = fromInt 2
        three = fromInt 3
    in
    suite "Add testsuite"
        [ equals (one `add` two) three
        ]

subTests : Test
subTests =
    let one = fromInt 1
        two = fromInt 2
        three = fromInt 3
    in
    suite "sub testsuite"
        [ equals (three `sub` two) one
        , equals (dnegate (dnegate one)) one
        ]

mulTests : Test
mulTests =
    let one = fromInt 1
        two = fromInt 2
        three = fromInt 3
        six = fromInt 6
    in
    suite "mul testsuite"
        [ equals (mul two three) six
        , equals (mul two (dnegate three)) (dnegate six)
        ]

fromStringTests : Test
fromStringTests =
    suite "fromString testsuite"
        [ equals (fromString "1") (Just (fromInt 1))
        , equals (fromString "-1") (Just (fromInt -1))
        , equals (fromString "-4521") (Just (fromInt -4521))
        , equals (fromString "sdfwe") (Nothing)
        , equals (fromString "300e0") (Just (fromInt 300))
        , equals (fromString "300E2") (Just (fromIntWithExponent 300 2))
        , equals (fromString "3.3") (Just (fromIntWithExponent 33 -1))
        ]

fromFloatTests : Test
fromFloatTests =
    suite "fromFloat testsuite"
        [ equals (fromFloat 1) (fromInt 1)
        , equals (fromFloat -1) (fromInt -1)
        , equals (fromFloat -4521) (fromInt -4521)
        , equals (fromFloat 3.3) (uFromString "3.3")
        , equals (fromFloat -3.4) (uFromString "-3.4")
        , equals (fromFloat 11e-1) (uFromString "1.1")
        ]

toStringTests : Test
toStringTests =
    suite "toString testsuite"
        [ equals (dtoString (fromInt 1)) "1"
        , equals (dtoString (fromInt -1)) "-1"
        , equals (dtoString (fromIntWithExponent -1 1)) "-10"
        , equals "0.1" (dtoString (fromIntWithExponent 1 -1)) 
        , equals "-0.1" (dtoString (fromIntWithExponent -1 -1))
        , equals "-1.1" (dtoString (fromIntWithExponent -11 -1))
        , equals "-1234.5678" (dtoString (fromIntWithExponent -12345678 -4))
        ]

fastdivTests : Test
fastdivTests =
    suite "fastdiv testsuite"
        [ equals "0.1" (dtoString ((fromInt 1) `fastdiv` (fromInt 10))) 
        , equals "0.3333333333333333" (dtoString ((fromInt 1) `fastdiv` (fromInt 3))) 
        , equals "0.6666666666666666" (dtoString ((fromInt 1200) `fastdiv` (fromInt 1800))) 
        ]

compareTests : Test
compareTests =
    suite "compare testsuite"
        [ equals (Just EQ) (dcompare (fromString "1") (Just (fromInt 1)))
        , equals (Just GT) (dcompare (fromString "10") (Just (fromInt 1)))
        , equals (Just LT) (dcompare (fromString "1") (Just (fromInt 10)))
        , equals (Just EQ) (dcompare (fromString "1.0") (Just (fromInt 1)))
        , equals (Just GT) (dcompare (fromString "1.0") (Just (fromInt -10)))
        ]

roundTests : Test
roundTests =
    suite "round testsuite"
        [ equals "0.3333" (dtoString (dround -4 ((fromInt 1) `fastdiv` (fromInt 3))))
        , equals "0.6667" (dtoString (dround -4 ((fromInt 1200) `fastdiv` (fromInt 1800))))
        , equals "-0.6667" (dtoString (dround -4 ((fromInt -1200) `fastdiv` (fromInt 1800))))
        , equals 3 (getDigit 2 (uFromString "12345"))
        , equals 0 (getDigit -1 (uFromString "12345"))
        , equals 3 (getDigit 0 (uFromString "123.45"))
        , equals 2 (getDigit 1 (uFromString "123.45"))
        , equals 4 (getDigit -1 (uFromString "123.45"))
        , equals 0 (getDigit 2 (uFromString "0.45"))
        ]

truncateTests : Test
truncateTests =
    suite "truncate testsuite"
        [ equals "0.3333" (dtoString (dtruncate -4 ((fromInt 1) `fastdiv` (fromInt 3))))
        , equals "0.6666" (dtoString (dtruncate -4 ((fromInt 1200) `fastdiv` (fromInt 1800))))
        , equals "-0.6666" (dtoString (dtruncate -4 ((fromInt -1200) `fastdiv` (fromInt 1800))))
        ]

allTests : Test
allTests =
    suite "All tests"
        [ addTests
        , subTests
        , mulTests
        , fromStringTests
        , fromFloatTests
        , toStringTests
        , fastdivTests
        , compareTests
        , roundTests
        , truncateTests
        ]

main : Element
main = 
    elementRunner allTests
