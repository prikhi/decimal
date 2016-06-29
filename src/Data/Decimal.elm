module Data.Decimal exposing
    ( Decimal
    , fromInt
    , fromIntWithExponent
    , fromString
    , unsafeFromString
    , toString
    , toFloat
    , fromFloat
    , add
    , sub
    , negate
    , mul
    , fastdiv
    , abs
    , gt
    , gte
    , eq
    , neq
    , lt
    , lte
    , compare
    , zero
    , one
    , minusOne
    , truncate
    , round
    , getDigit
    )


{-|

# The datatype
@docs Decimal

# From stuff
@docs fromInt
@docs fromIntWithExponent
@docs fromString
@docs fromFloat
@docs unsafeFromString

# To stuff
@docs toString
@docs toFloat

# Arithmetic operations
@docs add
@docs sub
@docs negate
@docs mul
@docs fastdiv

# Rounding
@docs truncate
@docs round

# Comparing
@docs gt
@docs gte
@docs eq
@docs neq
@docs lt
@docs lte
@docs compare

# Misc operations
@docs abs
@docs getDigit

# Common numbers
@docs zero
@docs one
@docs minusOne

-}

import Data.Integer
import Maybe exposing (Maybe)
import String
import Debug


type alias Mantissa = Data.Integer.Integer
type alias Exponent = Int

{-|
The Decimal data type
It is represented as mantissa * 10 ^ exponent
-}

type Decimal = Decimal Mantissa Exponent

{-|
Converts an Int to a Decimal
-}
fromInt : Int -> Decimal
fromInt n = fromIntWithExponent n 0

{-|
Converts an Int to a Decimal, but specifying the exponent
-}
fromIntWithExponent : Int -> Int -> Decimal
fromIntWithExponent n e =
    Decimal (Data.Integer.fromInt n) e

{-|
Converts a String to a Maybe Decimal. The string shall be in the format [<sign>]<numbers>[.<numbers>][e<numbers>]
-}
fromString : String -> Maybe Decimal
fromString s =
    let stringToDecimal s =
        let stringIntToDecimal s' e =
            case Data.Integer.fromString s' of
                Nothing -> Nothing
                Just(a) -> Just (Decimal a e)
        in
        case String.split "." s of
            [a, b] -> stringIntToDecimal (a ++ b) (-(String.length b))
            [a] -> stringIntToDecimal a 0
            _ -> Nothing
    in
    let makeMantissa s =
        case String.split "." s of
            [s1] -> stringToDecimal s1
            [s1, s2] -> stringToDecimal (String.join "" [s1, s2])
            _ -> Nothing
    in
    let splitMantissaExponent s =
        case String.split "e" (String.toLower s) of
            [s1] -> (stringToDecimal s1, Ok 0)
            [s1, s2] -> (stringToDecimal s1, String.toInt s2)
            _ -> (Nothing, Err "")
    in
    case splitMantissaExponent s of
        (Just (Decimal m a), Ok e) -> Just (Decimal m (e+a))
        _ -> Nothing

{-|
Converts a String to a Decimal,
but if the string does not represent
a valid Decimal, it crashes.
Useful for Decimal constants.
-}
unsafeFromString : String -> Decimal
unsafeFromString s =
    case fromString s of
        Just a -> a
        Nothing -> Debug.crash "Invalid string for Decimal"

insert_decimal_period : Int -> String -> String
insert_decimal_period pos s =
    let extra_zeros = pos - String.length s
        padded_s = if extra_zeros >= 0 then (String.repeat (extra_zeros+1) "0") ++ s else s
        before = String.dropRight pos padded_s
        after = String.right pos padded_s
    in
    before ++ "." ++ after

{-|
Converts a Decimal to a String
-}
toString : Decimal -> String
toString (Decimal m e) =
    let abs_m = if m `Data.Integer.gte` (Data.Integer.fromInt 0) then m else Data.Integer.negate m
        s = Data.Integer.toString abs_m
        sign = if m `Data.Integer.gte` (Data.Integer.fromInt 0) then "" else "-"
        add_zeros n = String.repeat n "0"
    in    
    case Basics.compare e 0 of
        EQ -> sign ++ s
        GT -> sign ++ s ++ (add_zeros e)
        LT -> sign ++ insert_decimal_period (0-e) s

{-|
Converts a Decimal to a Float
-}
toFloat : Decimal -> Float
toFloat d = 
    case String.toFloat (toString d) of
        Ok a -> a
        Err _ -> 42.0

{-|
Converts a Float to a Decimal
-}
fromFloat : Float -> Maybe Decimal
fromFloat f = fromString (Basics.toString f)

{-|
Fast and dirty division. Don't expect too much precision from this division. Dividing by zero is bad, and Nothing will be returned.
-}
fastdiv : Decimal -> Decimal -> Maybe Decimal
fastdiv a b =
    let fa = toFloat a
        fb = toFloat b
        res = fa / fb
    in
    fromFloat res

addDecimals : Int -> Decimal -> Decimal
addDecimals i (Decimal m e) =
    let mul10 x = x `Data.Integer.mul` (Data.Integer.fromInt 10) in
    if i == 0
    then Decimal m e
    else if i > 0
         then addDecimals (i - 1) (Decimal (mul10 m) (e - 1))
         else Decimal m e

toExponent : Exponent -> Decimal -> Decimal
toExponent e (Decimal md ed) = addDecimals (ed - e) (Decimal md ed)

toCommonExponent : (Decimal, Decimal) -> (Decimal, Decimal)
toCommonExponent (a, b) =
    let (Decimal ma ea) = a
        (Decimal mb eb) = b
        exponent = min ea eb
    in
    (toExponent exponent a, toExponent exponent b)

{-|
Addition
-}
add : Decimal -> Decimal -> Decimal
add a b =
    let (ra, rb) = toCommonExponent (a, b)
        (Decimal ma ea) = ra
        (Decimal mb eb) = rb in
    Decimal (ma `Data.Integer.add` mb) ea

{-|
Changes the sign of a Decimal
-}
negate : Decimal -> Decimal
negate (Decimal m e) = Decimal (Data.Integer.negate m) e

{-|
Substraction
-}
sub : Decimal -> Decimal -> Decimal
sub a b = add a (negate b)

{-|
Multiplication
-}
mul : Decimal -> Decimal -> Decimal
mul (Decimal ma ea) (Decimal mb eb) =
    Decimal (ma `Data.Integer.mul` mb) (ea + eb)

{-|
Absolute value (sets the sign as positive)
-}
abs : Decimal -> Decimal
abs (Decimal m e) =
    case Data.Integer.compare m (Data.Integer.fromInt 0) of
        LT -> Decimal (Data.Integer.negate m) e
        _ -> Decimal m e

{-|
Compares two Decimals
-}
compare : Decimal -> Decimal -> Order
compare a b =
    let (fa, fb) = toCommonExponent (a, b)
        (Decimal ma ea) = fa
        (Decimal mb eb) = fb
    in
    Data.Integer.compare ma mb

{-|
Equals
-}
eq : Decimal -> Decimal -> Bool
eq a b =
    case compare a b of
        EQ -> True
        _ -> False

{-|
Not equals
-}
neq : Decimal -> Decimal -> Bool
neq a b = not (eq a b)

{-|
Greater than
-}
gt : Decimal -> Decimal -> Bool
gt a b =
    case compare a b of
        GT -> True
        _ -> False

{-|
Greater than or equals
-}
gte : Decimal -> Decimal -> Bool
gte a b = (gt a b) || (eq a b)

{-|
Less than
-}
lt : Decimal -> Decimal -> Bool
lt a b =
    case compare a b of
        LT -> True
        _ -> False

{-|
Less than or equals
-}
lte : Decimal -> Decimal -> Bool
lte a b = (lt a b) || (eq a b)

{-|
The number 0
-}
zero : Decimal
zero = fromInt 0

{-|
The number 1
-}
one : Decimal
one = fromInt 1

{-|
The number -1
-}
minusOne : Decimal
minusOne = fromInt -1

{-|
True if the number is positive or zero
-}
isPositive : Decimal -> Bool
isPositive x = x `gte` zero

{-|
True if the number is negative (but not zero)
-}
isNegative : Decimal -> Bool
isNegative x = not (isPositive x)

{-|
Gets the specified digit from a Decimal. The digits are:
0 -> units
1 -> tens
2 -> hundreds
and so on
-1 -> tenths
-2 -> hundredths
and so on
-}
getDigit : Int -> Decimal -> Int
getDigit n d =
    let s = toString d in
    let toInt d =
        case d of
            "1" -> 1
            "2" -> 2
            "3" -> 3
            "4" -> 4
            "5" -> 5
            "6" -> 6
            "7" -> 7
            "8" -> 8
            "9" -> 9
            "0" -> 0
            "" -> 0
            _ -> -1
    in
    case (String.split "." s, Basics.compare n 0) of
        ([a], GT) ->
            toInt (String.right 1 (String.dropRight n a))
        ([a], EQ) ->
            toInt (String.right 1 a)
        ([a], LT) ->
            0
        ([a, b], GT) ->
            toInt (String.right 1 (String.dropRight n a))
        ([a, b], EQ) ->
            toInt (String.right 1 a)
        ([a, b], LT) ->
            toInt (String.left 1 (String.dropLeft (-n-1) b))
        _ -> -13

{-|
Truncates the Decimal to the specified decimal places
-}
truncate : Int -> Decimal -> Decimal
truncate n d =
    let s = toString d in
    let toDecimal s =
        case fromString s of
            Just a -> a
            Nothing -> zero
    in
    case (String.split "." s, n >= 0) of
        ([a], True) -> toDecimal (String.dropRight n a ++ String.repeat n "0")
        ([a], False) -> toDecimal a
        ([a, b], True) -> toDecimal (String.dropRight n a ++ String.repeat n "0")
        ([a, b], False) -> toDecimal (a ++ "." ++ String.left (-n)  b)
        _ -> zero

signAsInt : Decimal -> Int
signAsInt d =
    case compare d zero of
        LT -> -1
        EQ -> 0
        GT -> 1

{-|
Rounds the Decimal to the specified decimal places
-}
round : Int -> Decimal -> Decimal
round n d =
    let t = truncate n d
        next_digit = getDigit (n - 1) d
        to_increment = fromIntWithExponent (signAsInt d) n in
    if next_digit >= 5 then t `add` to_increment else t
