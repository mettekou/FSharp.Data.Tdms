namespace FSharp.Data.Tdms

open System.Runtime.CompilerServices
open System.Runtime.InteropServices

#if !IS_DESIGNTIME
[<Struct; IsReadOnly; StructLayout(LayoutKind.Sequential, Pack = 1)>]
#else
[<Struct; StructLayout(LayoutKind.Sequential, Pack = 1)>]
#endif
type Extended =
    { RawSignExponent: uint16
      RawSignificand: uint64 }

type float80 = Extended
