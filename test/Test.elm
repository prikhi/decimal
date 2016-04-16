import Data.Decimal exposing (..)
import Maybe exposing (Maybe, andThen)
import ElmTest exposing (..)
import Graphics.Element exposing (show, flow, down, Element, leftAligned)
import Check exposing (Claim, claim, that, is, for, quickCheck, true)
import Check.Producer exposing (list, int, Producer, char, ascii, filter, tuple, map, func2, rangeInt)
import Check.Test

decimal : Producer Decimal
decimal = map (\(a, b) -> fromIntWithExponent a b) (tuple (int, rangeInt (-20) 20))

liftMaybe : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe f a b =
    case (a, b) of
        (Just va, Just vb) -> Just (f va vb)
        _ -> Nothing

dtoString = Data.Decimal.toString
dnegate = Data.Decimal.negate
dcompare : Maybe Decimal -> Maybe Decimal -> Maybe Order
dcompare = liftMaybe Data.Decimal.compare
dtruncate n x = Just (Data.Decimal.truncate n x)
dround n x = Just (Data.Decimal.round n x)
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

qcAdd : Claim
qcAdd =
  Check.suite "Quickcheck Add"
    [ claim "Integer Conmutative adding"
      `that` (\(a, b) -> (fromInt a) `add` (fromInt b))
      `is` (\(a, b) -> (fromInt b) `add` (fromInt a))
      `for` tuple (int, int)
    , claim "Conmutative adding"
      `that` (\(a, b) -> a `add` b)
      `is` (\(a, b) -> b `add` a)
      `for` tuple (decimal, decimal)
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

qcSub : Claim
qcSub =
  Check.suite "Quickcheck Substract"
    [ claim "Integer conmutative substracting"
      `that` (\(a, b) -> (fromInt a) `sub` (fromInt b))
      `is` (\(a, b) -> dnegate ((fromInt b) `sub` (fromInt a)))
      `for` tuple (int, int)
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

qcMul : Claim
qcMul =
  Check.suite "Quickcheck multiplication"
    [ claim "Integer conmutative multiplication"
      `that` (\(a, b) -> (fromInt a) `mul` (fromInt b))
      `is` (\(a, b) -> (fromInt b) `mul` (fromInt a))
      `for` tuple (int, int)
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
        [ equals (fromFloat 1) (Just (fromInt 1))
        , equals (fromFloat -1) (Just (fromInt -1))
        , equals (fromFloat -4521) (Just (fromInt -4521))
        , equals (fromFloat 3.3) (fromString "3.3")
        , equals (fromFloat -3.4) (fromString "-3.4")
        , equals (fromFloat 11e-1) (fromString "1.1")
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
        [ equals (fromString "0.1") ((fromInt 1) `fastdiv` (fromInt 10))
        , equals (fromString "0.3333333333333333") ((fromInt 1) `fastdiv` (fromInt 3))
        , equals (fromString "0.6666666666666666") ((fromInt 1200) `fastdiv` (fromInt 1800))
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

qcCompare : Claim
qcCompare =
  Check.suite "Quickcheck compare"
    [ claim "Integer greater than"
      `that` (\a -> Data.Decimal.compare (fromInt a) (fromInt (a - 1)))
      `is` (\a -> GT)
      `for` int
    , claim "Integer equals"
      `that` (\a -> Data.Decimal.compare (fromInt a) (fromInt (a + 0)))
      `is` (\a -> EQ)
      `for` int
    , claim "Integer less than"
      `that` (\a -> Data.Decimal.compare (fromInt a) (fromInt (a + 1)))
      `is` (\a -> LT)
      `for` int
    ]


roundTests : Test
roundTests =
    suite "round testsuite"
        [ equals (fromString "0.3333") (((fromInt 1) `fastdiv` (fromInt 3)) `andThen` dround -4)
        , equals (fromString "0.6667") (((fromInt 1200) `fastdiv` (fromInt 1800)) `andThen` dround -4)
        , equals (fromString "-0.6667") (((fromInt -1200) `fastdiv` (fromInt 1800)) `andThen` dround -4)
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
        [ equals (fromString "0.3333") (((fromInt 1) `fastdiv` (fromInt 3)) `andThen` dtruncate -4)
        , equals (fromString "0.6666") (((fromInt 1200) `fastdiv` (fromInt 1800)) `andThen` dtruncate -4)
        , equals (fromString "-0.6666") (((fromInt -1200) `fastdiv` (fromInt 1800)) `andThen` dtruncate -4)
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
        , (Check.Test.evidenceToTest (quickCheck qcAdd))
        , (Check.Test.evidenceToTest (quickCheck qcSub))
        , (Check.Test.evidenceToTest (quickCheck qcMul))
        , (Check.Test.evidenceToTest (quickCheck qcCompare))
        ]

main : Element
main = 
    elementRunner allTests
