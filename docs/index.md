# TDMS 2.0 File Support for F# and C#

`FSharp.Data.Tdms` provides support for TDMS 2.0 files[<sup>[1]</sup>](#1) from F# and C# on .NET 6.0.
From F# it allows you to access these in a type-safe manner through a generative https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/[type provider], while plain old functions and methods are available to both F# and C#.
`FSharp.Data.Tdms` reads raw channel data from TDMS 2.0 files as arrays.

## Missing features

- Reading DAQmx raw data
- Defragmenting TDMS files
- Writing TDMS files

## License

`FSharp.Data.Tdms` is licensed under an MIT License.
Please refer to [`LICENSE`](https://github.com/mettekou/FSharp.Data.Tdms/blob/master/LICENSE) for its full text.

## Usage

### From F#

Using the generative type provider, given a TDMS file `Experiment.tdms` with a channel containing floating-point values named `Channel1` within a group named `Group1`:

```fsharp
#r "nuget: FSharp.Data.Tdms"

open FSharp.Data

[<Literal>]
let Path = "Experiment.tdms"

type Experiment = TdmsProvider<Path, WriteIndex = true>

let experiment = Experiment(Path)

experiment.Group1.Channel1.Data |> Array.iter (printfn "%f")
```

Alternatively, use the functions in the `FSharp.Data.Tdms` namespace, mainly those in the `File` module:

```fsharp
#r "nuget: FSharp.Data.Tdms"

open FSharp.Data.Tdms

File.read "Experiment.tdms" true
|> File.tryRawData<float> "Group1" "Channel1"
|> Option.defaultValue [||]
|> Array.iter (printfn "%f")
```

These functions have asynchronous equivalents:

```fsharp
#r "nuget: FSharp.Data.Tdms"

open FSharp.Control.Tasks.NonAffine
open FSharp.Data.Tdms

task {
let! file = File.readAsync "Experiment.tdms" true
let! data = File.tryRawDataAsync<float> "Group1" "Channel1"

    Option.defaultValue data
    |> Array.iter (printfn "%f")
}
|> Async.AwaitTask
|> Async.RunSynchronously
```

### From C#

When using `FSharp.Data.Tdms` from C#, prefer the API for idiomatic C#:

```csharp
using System;
using FSharp.Data.Tdms;

class Program
{
    static void Main(string[] args)
    {
        File.Read("Experiment.tdms", true).TryGetRawData("Group1", "Channel1", out double[] data);

        foreach (double sample in data)
        {
            Console.WriteLine(sample);
        }
    }
}
```

Or asynchronously:

```csharp
using System;
using FSharp.Data.Tdms;

class Program
{
    static async Task Main(string[] args)
    {
        var file = await File.ReadAsync("Experiment.tdms", true);
        var data = await file.TryGetRawDataAsync("Group1", "Channel1");

        foreach (double sample in data)
        {
            Console.WriteLine(sample);
        }
    }
}
```

### Property and raw data types

Most TDMS 2.0 data types directly map to .NET 5.0 data types.
The first exception is `tdsTypeTimeStamp`, which you can read as either link:FSharp.Data.Tdms/Timestamp.fs[`FSharp.Data.Tdms.Timestamp`] (this data type corresponds to https://www.ni.com/nl-be/support/documentation/supplemental/08/labview-timestamp-overview.html[the NI LabVIEW timestamp]), `System.DateTime`, `System.DateTimeOffset`, or `System.TimeSpan`.
In case of `System.TimeSpan`, `FSharp.Data.Tdms` returns the time elapsed since `01/01/1904 00:00:00.00 UTC`, as per https://www.ni.com/nl-be/support/documentation/supplemental/08/labview-timestamp-overview.html[this support document from NI].

The second exception is `tdsTypeExtendedFloat`.
Since .NET 5.0 does not support 80-bit extended precision floating point numbers, `FSharp.Data.Tdms` reads these as [`FSharp.Data.Tdms.Extended`](FSharp.Data.Tdms/Extended.fs) values.

Mapping from TDMS 2.0 to .NET 5.0 data types in `FSharp.Data.Tdms`

| Name | TDMS 2.0 data type | .NET 6.0 data type                                                                                                                                                                                                                                                                                                                                                                                                            | F# alias | C# alias |
|------|--------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------|----------|
| Void | `tdsTypeVoid` | [`FSharp.Core.Unit`](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-core-unit.html)                                                                                                                                                                                                                                                                                                                               | `unit` | None |
| 8-bit signed integer | `tdsTypeI8` | [`System.SByte`](https://docs.microsoft.com/en-us/dotnet/api/system.sbyte)                                                                                                                                                                                                                                                                                                                                                    | `int8` | `sbyte` |
| 16-bit signed integer | `tdsTypeI16` | [`System.Int16`](https://docs.microsoft.com/en-us/dotnet/api/system.int16)                                                                                                                                                                                                                                                                                                                                                    | `int16` | `short` |
| 32-bit signed integer | `tdsTypeI32` | [`System.Int32`](https://docs.microsoft.com/en-us/dotnet/api/system.int32)                                                                                                                                                                                                                                                                                                                                                    | `int` | `int` |
| 64-bit signed integer | `tdsTypeI64` | [`System.Int64`](https://docs.microsoft.com/en-us/dotnet/api/system.int64)                                                                                                                                                                                                                                                                                                                                                    | `int64` | `long` |
| 8-bit unsigned integer | `tdsTypeU8` | [`System.Byte`](https://docs.microsoft.com/en-us/dotnet/api/system.byte)                                                                                                                                                                                                                                                                                                                                                      | `uint8` | `byte` |
| 16-bit unsigned integer | `tdsTypeU16` | [`System.UInt16`](https://docs.microsoft.com/en-us/dotnet/api/system.uint16)                                                                                                                                                                                                                                                                                                                                                  | `uint16` | `ushort` |
| 32-bit unsigned integer | `tdsTypeU32` | [`System.UInt32`](https://docs.microsoft.com/en-us/dotnet/api/system.uint32)                                                                                                                                                                                                                                                                                                                                                  | `uint` | `uint` |
| 64-bit unsigned integer | `tdsTypeU64` | [`System.UInt64`](https://docs.microsoft.com/en-us/dotnet/api/system.uint64)                                                                                                                                                                                                                                                                                                                                                  | `uint64` | `ulong` |
| 32-bit single-precision floating point | <ul><li><code>tdsTypeSingleFloat</code></li><li><code>tdsTypeSingleFloatWithUnit</code></li></ul> | [`System.Single`](https://docs.microsoft.com/en-us/dotnet/api/system.single?view=net-5.0)                                                                                                                                                                                                                                                                                                                                     | `float32` | `float` |
| 64-bit double-precision floating point | <ul><li><code>tdsTypeDoubleFloat</code></li><li><code>tdsTypeDoubleFloatWithUnit</code></li></ul> | [`System.Double`](https://docs.microsoft.com/en-us/dotnet/api/system.double?view=net-5.0)                                                                                                                                                                                                                                                                                                                                     | `float` | `double` |
| 80-bit extended-precision floating point | <ul><li><code>tdsTypeExtendedFloat</code></li><li><code>tdsTypeExtendedFloatWithUnit</code></li></ul> | [`FSharp.Data.Tdms.Extended`](FSharp.Data.Tdms/Extended.fs)                                                                                                                                                                                                                                                                                                                                                                   | `float80` | None |
| Character string | `tdsTypeString` | [`System.String`](https://docs.microsoft.com/en-us/dotnet/api/system.string)                                                                                                                                                                                                                                                                                                                                                  | `string` | `string` |
| Boolean | `tdsTypeBoolean` | (https://docs.microsoft.com/en-us/dotnet/api/system.boolean?view=net-5.0[`System.Boolean`]                                                                                                                                                                                                                                                                                                                                    | `bool` | `bool` |
| Timestamp  | `tdsTypeTimeStamp` | <ul><li><a href="FSharp.Data.Tdms/Timestamp.fs"><code>FSharp.Data.Tdms.Timestamp</code></a></li><li>https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0[`System.DateTime`]</li><li>https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=net-5.0[`System.DateTimeOffset`]</li><li>https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=net-5.0[`System.TimeSpan`]</li></ul> | None | None |
| 32-bit single-precision floating point complex | `tdsTypeComplexSingleFloat` | https://docs.microsoft.com/en-us/dotnet/api/system.valuetuple-2?view=net-5.0[`System.ValueTuple<System.Single, System.Single>`]                                                                                                                                                                                                                                                                                               | `struct (float32 * float32)` | `(float, float)` |
| 64-bit double-precision floating point complex | `tdsTypeComplexDoubleFloat` | https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex?view=net-5.0[`System.Numerics.Complex`]                                                                                                                                                                                                                                                                                                                   | None | None |



### How to contribute

Imposter syndrome disclaimer: I want your help. No really, I do.

There might be a little voice inside that tells you you're not ready; that you need to do one more tutorial, or learn another framework, or write a few more blog posts before you can help me with this project.

I assure you, that's not the case.

And you don't just have to write code. You can help out by writing documentation, tests, or even by giving feedback about this work. (And yes, that includes giving feedback about the contribution guidelines.)

Thank you for contributing!

## References

<a id="1">[1]</a> National Instruments. 2019. The NI TDMS File Format. (January 2019). Retrieved January 12, 2019 from <a href="http://www.ni.com/white-paper/3727/en/"><code>http://www.ni.com/white-paper/3727/en/</code></a>.
