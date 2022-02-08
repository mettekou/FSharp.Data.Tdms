namespace FSharp.Data.Tdms

open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<Struct; IsReadOnly; StructLayout(LayoutKind.Sequential, Pack = 1)>]
type Extended =
    { RawSignExponent: uint16
      RawSignificand: uint64 }

type float80 = Extended
