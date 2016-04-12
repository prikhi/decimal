module Data.Decimal
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
    ) where


{-|

@docs Decimal


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

fromInt : Int -> Decimal
fromInt n = fromIntWithExponent n 0

fromIntWithExponent : Int -> Int -> Decimal
fromIntWithExponent n e =
    Decimal (Data.Integer.fromInt n) e

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

toString : Decimal -> String
toString (Decimal m e) =
    let abs_m = if m `Data.Integer.gte` (Data.Integer.fromInt 0) then m else Data.Integer.opposite m
        s = Data.Integer.toString abs_m
        sign = if m `Data.Integer.gte` (Data.Integer.fromInt 0) then "" else "-"
        add_zeros n = String.repeat n "0"
    in    
    case Basics.compare e 0 of
        EQ -> sign ++ s
        GT -> sign ++ s ++ (add_zeros e)
        LT -> sign ++ insert_decimal_period (0-e) s

toFloat : Decimal -> Float
toFloat d = 
    case String.toFloat (toString d) of
        Ok a -> a
        Err _ -> 42.0

fromFloat : Float -> Decimal
fromFloat f =
    case fromString (Basics.toString f) of
        Just a -> a
        Nothing -> fromInt 42

fastdiv : Decimal -> Decimal -> Decimal
fastdiv a b =
    let fa = toFloat a
        fb = toFloat b
        res = fa / fb
    in
    fromFloat res 

{-roundToExponent : Exponent -> Decimal -> Decimal-}

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

add : Decimal -> Decimal -> Decimal
add a b =
    let (ra, rb) = toCommonExponent (a, b)
        (Decimal ma ea) = ra
        (Decimal mb eb) = rb in
    Decimal (ma `Data.Integer.add` mb) ea

negate : Decimal -> Decimal
negate (Decimal m e) = Decimal (Data.Integer.opposite m) e

sub : Decimal -> Decimal -> Decimal
sub a b = add a (negate b)

mul : Decimal -> Decimal -> Decimal
mul (Decimal ma ea) (Decimal mb eb) =
    Decimal (ma `Data.Integer.mul` mb) (ea + eb)

abs : Decimal -> Decimal
abs (Decimal m e) =
    case Data.Integer.compare m (Data.Integer.fromInt 0) of
        LT -> Decimal (Data.Integer.opposite m) e
        _ -> Decimal m e

compare : Decimal -> Decimal -> Order
compare a b =
    let (fa, fb) = toCommonExponent (a, b)
        (Decimal ma ea) = fa
        (Decimal mb eb) = fb
    in
    Data.Integer.compare ma mb

eq : Decimal -> Decimal -> Bool
eq a b =
    case compare a b of
        EQ -> True
        _ -> False

neq : Decimal -> Decimal -> Bool
neq a b = not (eq a b)

gt : Decimal -> Decimal -> Bool
gt a b =
    case compare a b of
        GT -> True
        _ -> False

gte : Decimal -> Decimal -> Bool
gte a b = (gt a b) || (eq a b)

lt : Decimal -> Decimal -> Bool
lt a b =
    case compare a b of
        LT -> True
        _ -> False

lte : Decimal -> Decimal -> Bool
lte a b = (lt a b) || (eq a b)

zero : Decimal
zero = fromInt 0

one : Decimal
one = fromInt 1

minusOne : Decimal
minusOne = fromInt -1

isPositive : Decimal -> Bool
isPositive x = x `gte` zero

isNegative : Decimal -> Bool
isNegative x = not (isPositive x)

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

round : Int -> Decimal -> Decimal
round n d =
    let t = truncate n d
        next_digit = getDigit (n - 1) d
        to_increment = fromIntWithExponent (signAsInt d) n in
    if next_digit >= 5 then t `add` to_increment else t
