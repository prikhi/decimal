module Data.Decimal
    ( Decimal
    , fromInt
    , fromIntWithExponent
    , fromString
    , add
    , sub
    , negate
    , mul
    ) where


{-|

@docs Decimal


-}
import Data.Integer
import Maybe exposing (Maybe)
import String


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
        case Data.Integer.fromString s of
            Nothing -> Nothing
            Just(a) -> Just (Decimal a 0)
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
        (Just (Decimal m a), Ok e) -> Just (Decimal m e)
        _ -> Nothing
    
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
