# `FSharp.Data.Tdms` [![](https://buildstats.info/nuget/FSharp.Data.Tdms?includePreReleases=true)](https://www.nuget.org/packages/FSharp.Data.Tdms)

`FSharp.Data.Tdms` provides support for TDMS 2.0 files<span
id="the-ni-tdms-file-format"></span>&lt;&lt;#the-ni-tdms-file-format-entry,<sup>\[1\]</sup>&gt;&gt;
from F# and C# on .NET 6 and later. From F# it allows you to access these in a
type-safe manner through a generative [type
provider](https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/),
while plain old functions and methods are available to both F# and C#.
`FSharp.Data.Tdms` reads raw channel data from TDMS 2.0 files as arrays.

## Missing features

- Reading DAQmx raw data

- Defragmenting TDMS files

- Writing TDMS files

## License

`FSharp.Data.Tdms` is licensed under an MIT License. Please refer to
[`LICENSE`](https://github.com/mettekou/FSharp.Data.Tdms/blob/master/LICENSE)
for its full text.

## Installation

### .NET CLI

To add `FSharp.Data.Tdms` to a project using the .NET CLI, run the command:

```sh
dotnet add package FSharp.Data.Tdms --version 1.0.0-alpha.111
```

Be sure to replace `1.0.0-alpha.111` by the version you want to install.

### Interactive

For interactive use, either in C# or F#, add the following directive to your script file or enter it into your REPL (e.g. `dotnet fsi`):

```fsharp
#r "nuget: FSharp.Data.Tdms, 1.0.0-alpha.111"
```

Be sure to replace `1.0.0-alpha.111` by the version you want to use.

## Usage

### From F#

Using the generative type provider, given a TDMS file `Experiment.tdms`
with a channel containing floating-point values named `Channel1` within
a group named `Group1`:

```fsharp
#r "nuget: FSharp.Data.Tdms"

open FSharp.Data

[<Literal>]
let Path = "Experiment.tdms"

type Experiment = TdmsProvider<Path, WriteIndex = true>

let experiment = Experiment(Path)

experiment.Group1.Channel1.Data |> Array.iter (printfn "%f")
```

Alternatively, use the functions in the `FSharp.Data.Tdms` namespace,
mainly those in the `File` module:

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

Most TDMS 2.0 data types directly map to .NET data types. The first
exception is `tdsTypeTimeStamp`, which you can read as either
[`FSharp.Data.Tdms.Timestamp`](FSharp.Data.Tdms/Timestamp.fs) (this data
type corresponds to [the NI LabVIEW
timestamp](https://www.ni.com/nl-be/support/documentation/supplemental/08/labview-timestamp-overview.html)),
`System.DateTime`, `System.DateTimeOffset`, or `System.TimeSpan`. In
case of `System.TimeSpan`, `FSharp.Data.Tdms` returns the time elapsed
since `01/01/1904 00:00:00.00 UTC`, as per [this support document from
NI](https://www.ni.com/nl-be/support/documentation/supplemental/08/labview-timestamp-overview.html).

The second exception is `tdsTypeExtendedFloat`. Since .NET do not
support 80-bit extended precision floating point numbers,
`FSharp.Data.Tdms` reads these as
[`FSharp.Data.Tdms.Extended`](FSharp.Data.Tdms/Extended.fs) values.

<table>
<caption>Mapping from TDMS 2.0 to .NET data types in
<code>FSharp.Data.Tdms</code></caption>
<colgroup>
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
</colgroup>
<tbody>
<tr class="odd">
<td style="text-align: left;"><p>Name</p></td>
<td style="text-align: left;"><p>TDMS 2.0 data type</p></td>
<td style="text-align: left;"><p>.NET data type</p></td>
<td style="text-align: left;"><p>F# alias</p></td>
<td style="text-align: left;"><p>C# alias</p></td>
</tr>
<tr class="even">
<td style="text-align: left;"><p>Void</p></td>
<td style="text-align: left;"><p><code>tdsTypeVoid</code></p></td>
<td style="text-align: left;"><p><a
href="https://fsharp.github.io/fsharp-core-docs/reference/fsharp-core-unit.html"><code>FSharp.Core.Unit</code></a></p></td>
<td style="text-align: left;"><p><code>unit</code></p></td>
<td style="text-align: left;"><p>None</p></td>
</tr>
<tr class="odd">
<td style="text-align: left;"><p>8-bit signed integer</p></td>
<td style="text-align: left;"><p><code>tdsTypeI8</code></p></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.sbyte"><code>System.SByte</code></a></p></td>
<td style="text-align: left;"><p><code>int8</code></p></td>
<td style="text-align: left;"><p><code>sbyte</code></p></td>
</tr>
<tr class="even">
<td style="text-align: left;"><p>16-bit signed integer</p></td>
<td style="text-align: left;"><p><code>tdsTypeI16</code></p></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.int16"><code>System.Int16</code></a></p></td>
<td style="text-align: left;"><p><code>int16</code></p></td>
<td style="text-align: left;"><p><code>short</code></p></td>
</tr>
<tr class="odd">
<td style="text-align: left;"><p>32-bit signed integer</p></td>
<td style="text-align: left;"><p><code>tdsTypeI32</code></p></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.int32"><code>System.Int32</code></a></p></td>
<td style="text-align: left;"><p><code>int</code></p></td>
<td style="text-align: left;"><p><code>int</code></p></td>
</tr>
<tr class="even">
<td style="text-align: left;"><p>64-bit signed integer</p></td>
<td style="text-align: left;"><p><code>tdsTypeI64</code></p></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.int64"><code>System.Int64</code></a></p></td>
<td style="text-align: left;"><p><code>int64</code></p></td>
<td style="text-align: left;"><p><code>long</code></p></td>
</tr>
<tr class="odd">
<td style="text-align: left;"><p>8-bit unsigned integer</p></td>
<td style="text-align: left;"><p><code>tdsTypeU8</code></p></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.byte"><code>System.Byte</code></a></p></td>
<td style="text-align: left;"><p><code>uint8</code></p></td>
<td style="text-align: left;"><p><code>byte</code></p></td>
</tr>
<tr class="even">
<td style="text-align: left;"><p>16-bit unsigned integer</p></td>
<td style="text-align: left;"><p><code>tdsTypeU16</code></p></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.uint16"><code>System.UInt16</code></a></p></td>
<td style="text-align: left;"><p><code>uint16</code></p></td>
<td style="text-align: left;"><p><code>ushort</code></p></td>
</tr>
<tr class="odd">
<td style="text-align: left;"><p>32-bit unsigned integer</p></td>
<td style="text-align: left;"><p><code>tdsTypeU32</code></p></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.uint32"><code>System.UInt32</code></a></p></td>
<td style="text-align: left;"><p><code>uint</code></p></td>
<td style="text-align: left;"><p><code>uint</code></p></td>
</tr>
<tr class="even">
<td style="text-align: left;"><p>64-bit unsigned integer</p></td>
<td style="text-align: left;"><p><code>tdsTypeU64</code></p></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.uint64"><code>System.UInt64</code></a></p></td>
<td style="text-align: left;"><p><code>uint64</code></p></td>
<td style="text-align: left;"><p><code>ulong</code></p></td>
</tr>
<tr class="odd">
<td style="text-align: left;"><p>32-bit single-precision floating
point</p></td>
<td style="text-align: left;"><ul>
<li><p><code>tdsTypeSingleFloat</code></p></li>
<li><p><code>tdsTypeSingleFloatWithUnit</code></p></li>
</ul></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.single?view=net-5.0"><code>System.Single</code></a></p></td>
<td style="text-align: left;"><p><code>float32</code></p></td>
<td style="text-align: left;"><p><code>float</code></p></td>
</tr>
<tr class="even">
<td style="text-align: left;"><p>64-bit double-precision floating
point</p></td>
<td style="text-align: left;"><ul>
<li><p><code>tdsTypeDoubleFloat</code></p></li>
<li><p><code>tdsTypeDoubleFloatWithUnit</code></p></li>
</ul></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.double?view=net-5.0"><code>System.Double</code></a></p></td>
<td style="text-align: left;"><p><code>float</code></p></td>
<td style="text-align: left;"><p><code>double</code></p></td>
</tr>
<tr class="odd">
<td style="text-align: left;"><p>80-bit extended-precision floating
point</p></td>
<td style="text-align: left;"><ul>
<li><p><code>tdsTypeExtendedFloat</code></p></li>
<li><p><code>tdsTypeExtendedFloatWithUnit</code></p></li>
</ul></td>
<td style="text-align: left;"><p><a
href="FSharp.Data.Tdms/Extended.fs"><code>FSharp.Data.Tdms.Extended</code></a></p></td>
<td style="text-align: left;"><p><code>float80</code></p></td>
<td style="text-align: left;"><p>None</p></td>
</tr>
<tr class="even">
<td style="text-align: left;"><p>Character string</p></td>
<td style="text-align: left;"><p><code>tdsTypeString</code></p></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.string"><code>System.String</code></a></p></td>
<td style="text-align: left;"><p><code>string</code></p></td>
<td style="text-align: left;"><p><code>string</code></p></td>
</tr>
<tr class="odd">
<td style="text-align: left;"><p>Boolean</p></td>
<td style="text-align: left;"><p><code>tdsTypeBoolean</code></p></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.boolean?view=net-5.0"><code>System.Boolean</code></a></p></td>
<td style="text-align: left;"><p><code>bool</code></p></td>
<td style="text-align: left;"><p><code>bool</code></p></td>
</tr>
<tr class="even">
<td style="text-align: left;"><p>Timestamp</p></td>
<td style="text-align: left;"><p><code>tdsTypeTimeStamp</code></p></td>
<td style="text-align: left;"><ul>
<li><p><a
href="FSharp.Data.Tdms/Timestamp.fs"><code>FSharp.Data.Tdms.Timestamp</code></a></p></li>
<li><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0"><code>System.DateTime</code></a></p></li>
<li><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=net-5.0"><code>System.DateTimeOffset</code></a></p></li>
<li><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=net-5.0"><code>System.TimeSpan</code></a></p></li>
</ul></td>
<td style="text-align: left;"><p>None</p></td>
<td style="text-align: left;"><p>None</p></td>
</tr>
<tr class="odd">
<td style="text-align: left;"><p>32-bit single-precision floating point
complex</p></td>
<td
style="text-align: left;"><p><code>tdsTypeComplexSingleFloat</code></p></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.valuetuple-2?view=net-5.0"><code>System.ValueTuple&lt;System.Single, System.Single&gt;</code></a></p></td>
<td
style="text-align: left;"><p><code>struct (float32 * float32)</code></p></td>
<td style="text-align: left;"><p><code>(float, float)</code></p></td>
</tr>
<tr class="even">
<td style="text-align: left;"><p>64-bit double-precision floating point
complex</p></td>
<td
style="text-align: left;"><p><code>tdsTypeComplexDoubleFloat</code></p></td>
<td style="text-align: left;"><p><a
href="https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex?view=net-5.0"><code>System.Numerics.Complex</code></a></p></td>
<td style="text-align: left;"><p>None</p></td>
<td style="text-align: left;"><p>None</p></td>
</tr>
</tbody>
</table>

## Performance

The [BenchmarkDotNet](https://benchmarkdotnet.org) benchmarks in this
section give an idea of the performance of `FSharp.Data.Tdms` when
compared to [`TDMSReader`](https://github.com/mikeobrien/TDMSReader),
the only other TDMS 2.0 implementation which works on .NET 6 and later. Since
`TDMSReader` does not support reading TDMS index files, the benchmark
disables this feature for `FSharp.Data.Tdms` as well, for a fair
comparison. This means that `FSharp.Data.Tdms` may perform better in
practice for TDMS files with many raw data segments.

### Small file

This benchmark reads 30,489 double-precision floating points from a segmented 3.1 MB TDMS 2.0 file.

```ini

BenchmarkDotNet=v0.13.2, OS=opensuse-tumbleweed 20221223
AMD Ryzen 9 5950X, 1 CPU, 32 logical and 16 physical cores
.NET SDK=7.0.101
  [Host]   : .NET 7.0.1 (7.0.122.56804), X64 RyuJIT AVX2 DEBUG
  .NET 7.0 : .NET 7.0.1 (7.0.122.56804), X64 RyuJIT AVX2

Job=.NET 7.0  Runtime=.NET 7.0

```

| Method              |       Mean |    Error |   StdDev | Ratio |
| ------------------- | ---------: | -------: | -------: | ----: |
| TDMSReader          | 1,677.4 μs |  3.55 μs |  2.96 μs |  1.00 |
| FSharpDataTdms      |   741.4 μs |  4.04 μs |  3.78 μs |  0.44 |
| FSharpDataTdmsAsync |   882.0 μs | 13.53 μs | 12.65 μs |  0.52 |

### Medium-sized file

This benchmark reads a channel of 43,200 strings from a segmented 138.1 MB TDMS 2.0 file.

```ini

BenchmarkDotNet=v0.13.2, OS=opensuse-tumbleweed 20221223
AMD Ryzen 9 5950X, 1 CPU, 32 logical and 16 physical cores
.NET SDK=7.0.101
  [Host]   : .NET 7.0.1 (7.0.122.56804), X64 RyuJIT AVX2 DEBUG
  .NET 7.0 : .NET 7.0.1 (7.0.122.56804), X64 RyuJIT AVX2

Job=.NET 7.0  Runtime=.NET 7.0

```

| Method              |    Mean |    Error |   StdDev | Ratio |
| ------------------- | ------: | -------: | -------: | ----: |
| TDMSReader          | 8.570 s | 0.0539 s | 0.0505 s |  1.00 |
| FSharpDataTdms      | 2.581 s | 0.0105 s | 0.0098 s |  0.30 |
| FSharpDataTdmsAsync | 2.691 s | 0.0526 s | 0.0516 s |  0.31 |

### Large file

This benchmark reads a channel of 779,297 double-precision floating points from a segmented 1.54 GB TDMS 2.0 file.

```ini

BenchmarkDotNet=v0.13.2, OS=opensuse-tumbleweed 20221223
AMD Ryzen 9 5950X, 1 CPU, 32 logical and 16 physical cores
.NET SDK=7.0.101
  [Host]   : .NET 7.0.1 (7.0.122.56804), X64 RyuJIT AVX2 DEBUG
  .NET 7.0 : .NET 7.0.1 (7.0.122.56804), X64 RyuJIT AVX2

Job=.NET 7.0  Runtime=.NET 7.0

```

| Method              |     Mean |   Error |  StdDev | Ratio |
| ------------------- | -------: | ------: | ------: | ----: |
| TDMSReader          | 996.0 ms | 8.77 ms | 7.77 ms |  1.00 |
| FSharpDataTdms      | 522.1 ms | 5.81 ms | 5.43 ms |  0.52 |
| FSharpDataTdmsAsync | 654.5 ms | 6.84 ms | 6.07 ms |  0.66 |

## How to contribute

Imposter syndrome disclaimer: I want your help. No really, I do.

There might be a little voice inside that tells you you’re not ready;
that you need to do one more tutorial, or learn another framework, or
write a few more blog posts before you can help me with this project.

I assure you, that’s not the case.

And you don’t just have to write code. You can help out by writing
documentation, tests, or even by giving feedback about this work. (And
yes, that includes giving feedback about the contribution guidelines.)

Thank you for contributing!

## References

<span
id="the-ni-tdms-file-format-entry"></span>&lt;&lt;#the-ni-tdms-file-format,<sup>\[1\]</sup>&gt;&gt;
National Instruments. 2019. The NI TDMS File Format. (January 2019).
Retrieved January 12, 2019 from
`http://www.ni.com/white-paper/3727/en/`.
