module Tests exposing (..)

import Expect
import Fuzz exposing (Fuzzer, int, intRange, float)
import Test exposing (..)
import Decimal as D


decimal : Fuzzer D.Decimal
decimal =
    Fuzz.map2 D.fromIntWithExponent int (intRange -20 20)


addTests : Test
addTests =
    describe "Decimal.add"
        [ fuzz2 int int "mirrors normal addition" <|
            \a b ->
                Expect.equal (D.add (D.fromInt a) (D.fromInt b)) (D.fromInt <| a + b)
        , fuzz2 decimal decimal "is commutative" <|
            \a b ->
                Expect.equal (D.add a b) (D.add b a)
        ]


subTests : Test
subTests =
    describe "Decimal.sub"
        [ fuzz2 int int "mirrors normal subtraction" <|
            \a b ->
                Expect.equal (D.sub (D.fromInt a) (D.fromInt b)) (D.fromInt <| a - b)
        , fuzz2 decimal decimal "switching orders is the same as the negation" <|
            \a b ->
                Expect.equal (D.sub a b) (D.negate (D.sub b a))
        ]


mulTests : Test
mulTests =
    let
        safeInt =
            intRange -46340 46340
    in
        describe "Decimal.mul"
            [ fuzz2 safeInt safeInt "mirrors normal multiplication" <|
                \a b ->
                    Expect.true "Expected multiplication to mimic integer multiplication" <|
                        D.eq (D.mul (D.fromInt a) (D.fromInt b)) (D.fromInt <| a * b)
            , fuzz2 decimal decimal "is commutative" <|
                \a b ->
                    Expect.equal (D.mul a b) (D.mul b a)
            ]


fromStringTests : Test
fromStringTests =
    describe "Decimal.fromString"
        [ test "positive integer" <|
            \_ ->
                Expect.equal (D.fromString "1") (Just <| D.fromInt 1)
        , test "negative integer" <|
            \_ ->
                Expect.equal (D.fromString "-1") (Just <| D.fromInt -1)
        , test "zero" <|
            \_ ->
                Expect.equal (D.fromString "0") (Just <| D.fromInt 0)
        , test "non-number" <|
            \_ ->
                Expect.equal (D.fromString "esdf") Nothing
        , fuzz2 int int "exponent" <|
            \a b ->
                Expect.equal (D.fromString <| String.fromInt a ++ "e" ++ String.fromInt b)
                    (Just <| D.fromIntWithExponent a b)
        , test "decimal" <|
            \_ ->
                Expect.equal (D.fromString "1.1") (Just <| D.fromIntWithExponent 11 -1)
        ]


fromFloatTests : Test
fromFloatTests =
    describe "Decimal.fromFloat"
        [ test "positive float" <|
            \_ ->
                Expect.equal (D.fromFloat 1) (Just <| D.fromInt 1)
        , test "negative float" <|
            \_ ->
                Expect.equal (D.fromFloat -1) (Just <| D.fromInt -1)
        , test "zero" <|
            \_ ->
                Expect.equal (D.fromFloat 0) (Just <| D.fromInt 0)
        , test "decimal" <|
            \_ ->
                Expect.equal (D.fromFloat 3.3) (D.fromString "3.3")
        , test "exponent" <|
            \_ ->
                Expect.equal (D.fromFloat 1.1e0) (D.fromString "1.1e0")
        , fuzz float "equivalent to fromString" <|
            \a ->
                Expect.equal (D.fromFloat a) (D.fromString <| String.fromFloat a)
        ]


toStringTests : Test
toStringTests =
    describe "Decimal.toString"
        [ test "positive" <|
            \_ ->
                Expect.equal "1" (D.toString <| D.fromInt 1)
        , test "zero" <|
            \_ ->
                Expect.equal "0" (D.toString <| D.fromInt 0)
        , test "negative" <|
            \_ ->
                Expect.equal "-1" (D.toString <| D.fromInt -1)
        , test "decimal" <|
            \_ ->
                Expect.equal "-1234.5678" (D.toString <| D.fromIntWithExponent -12345678 -4)
        ]


fastdivTests : Test
fastdivTests =
    describe "Decimal.fastdiv"
        [ test "terminating division" <|
            \_ ->
                Expect.equal (D.fromString "0.1") (D.fastdiv (D.fromInt 1) (D.fromInt 10))
        , test "repeating division" <|
            \_ ->
                Expect.equal (D.fromString "0.3333333333333333") (D.fastdiv (D.fromInt 1) (D.fromInt 3))
        , test "repeating, reducable division" <|
            \_ ->
                Expect.equal (D.fromString "0.6666666666666666") (D.fastdiv (D.fromInt 1200) (D.fromInt 1800))
        ]


compareTests : Test
compareTests =
    describe "Decimal.compare"
        [ fuzz int "integer equality" <|
            \a ->
                Expect.equal EQ (D.compare (D.fromInt a) (D.fromInt a))
        , fuzz int "integer less than" <|
            \a ->
                Expect.equal LT (D.compare (D.fromInt a) (D.fromInt <| a + 1))
        , fuzz int "integer greater than" <|
            \a ->
                Expect.equal GT (D.compare (D.fromInt a) (D.fromInt <| a - 1))
        ]


roundTests : Test
roundTests =
    describe "Decimal.round"
        [ test "repeating" <|
            \_ ->
                Expect.equal (D.fromString "0.3333")
                    (D.fastdiv (D.fromInt 1) (D.fromInt 3) |> Maybe.map (D.round -4))
        , test "repeating & reducible" <|
            \_ ->
                Expect.equal (D.fromString "0.6667")
                    (D.fastdiv (D.fromInt 1200) (D.fromInt 1800) |> Maybe.map (D.round -4))
        , test "negative repeating & reducible" <|
            \_ ->
                Expect.equal (D.fromString "-0.6667")
                    (D.fastdiv (D.fromInt -1200) (D.fromInt 1800) |> Maybe.map (D.round -4))
        ]


getDigitTests : Test
getDigitTests =
    describe "Decimal.getDigit"
        [ test "positive digit of integer" <|
            \_ ->
                Expect.equal 3 (D.getDigit 2 (D.fromInt 12345))
        , test "negative digit of integer" <|
            \_ ->
                Expect.equal 0 (D.getDigit -1 (D.fromInt 12345))
        , test "ones place of decimal" <|
            \_ ->
                Expect.equal (Just 3) (Maybe.map (D.getDigit 0) (D.fromString "123.45"))
        , test "positive digit of decimal" <|
            \_ ->
                Expect.equal (Just 2) (Maybe.map (D.getDigit 1) (D.fromString "123.45"))
        , test "negative digit of decimal" <|
            \_ ->
                Expect.equal (Just 4) (Maybe.map (D.getDigit -1) (D.fromString "123.45"))
        , test "positive digit of fraction" <|
            \_ ->
                Expect.equal (Just 0) (Maybe.map (D.getDigit 2) (D.fromString "0.45"))
        ]


truncateTests : Test
truncateTests =
    describe "Decimal.truncate"
        [ test "naturally rounds down" <|
            \_ ->
                Expect.equal (D.fromString "0.3333")
                    (D.fastdiv (D.fromInt 1) (D.fromInt 3) |> Maybe.map (D.truncate -4))
        , test "naturally rounds up" <|
            \_ ->
                Expect.equal (D.fromString "0.6666")
                    (D.fastdiv (D.fromInt 1200) (D.fromInt 1800) |> Maybe.map (D.truncate -4))
        , test "negative" <|
            \_ ->
                Expect.equal (D.fromString "-0.6666")
                    (D.fastdiv (D.fromInt -1200) (D.fromInt 1800) |> Maybe.map (D.truncate -4))
        ]
