namespace FSharp.Data.Tdms

open System
open System.Runtime.CompilerServices

[<Struct; IsReadOnly>]
type Timestamp =
    { FractionsOfASecond: uint64
      SecondsSinceNiEpoch: int64 }

module Timestamp =
  
  let niEpochDateTime = DateTime(1904, 1, 1, 0, 0, 0, DateTimeKind.Utc)

  let niEpochDateTimeOffset = DateTimeOffset(1904, 1, 1, 0, 0, 0, TimeSpan.Zero)

  let toTimeSpan { SecondsSinceNiEpoch = secondsSinceNiEpoch; FractionsOfASecond = fractionsOfASecond } =
    TimeSpan.FromSeconds (float secondsSinceNiEpoch) + TimeSpan.FromSeconds (float fractionsOfASecond / float UInt64.MaxValue)

  let toDateTime timestamp = (niEpochDateTime + toTimeSpan timestamp).ToLocalTime()

  let toDateTimeOffset timestamp = (niEpochDateTimeOffset + toTimeSpan timestamp).ToLocalTime()