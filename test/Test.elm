import Data.Decimal exposing (..)
import Maybe exposing (Maybe)
import ElmTest exposing (..)
import Graphics.Element exposing (show, flow, down, Element, leftAligned)


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
        , equals (Data.Decimal.negate (Data.Decimal.negate one)) one
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
        , equals (mul two (Data.Decimal.negate three)) (Data.Decimal.negate six)
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
        ]

allTests : Test
allTests =
    suite "All tests"
        [ addTests
        , subTests
        , mulTests
        , fromStringTests
        ]

main : Element
main = 
    elementRunner allTests
