namespace FSharp.Data.Tdms

open System.Runtime.CompilerServices

[<Struct; IsReadOnly>]
type Extended =
    { RawSignExponent: uint16
      RawSignificand: uint64 }

type float80 = Extended