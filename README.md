# Decimal

[![Build Status](https://travis-ci.org/prikhi/decimal.svg?branch=master)](https://travis-ci.org/prikhi/decimal)


Arbitrary-precision Decimal numbers for Elm.

```elm
import Decimal exposing (Decimal)

centsToDecimalString : Int -> String
centsToDecimalString totalCents =
    Decimal.fromInt totalCents
        |> Decimal.mul (Decimal.intWithExponent 1 -2)
        |> Decimal.toString
        |> ((++) "$")
```


# License

MIT

Forked from [javcasas' elm-decimal](https://github.com/javcasas/elm-decimal)
