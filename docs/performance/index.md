## Performance

The [BenchmarkDotNet](https://benchmarkdotnet.org) benchmarks in this section give an idea of the performance of `FSharp.Data.Tdms` when compared to [`TDMSReader`](https://github.com/mikeobrien/TDMSReader), the only other TDMS 2.0 implementation which works on .NET 6.0.
Since `TDMSReader` does not support reading TDMS index files, the benchmark disables this feature for `FSharp.Data.Tdms` as well, for a fair comparison.
This means that `FSharp.Data.Tdms` may perform better in practice for TDMS files with many raw data segments.

### Small file

This benchmark reads 30,489 double-precision floating points from a segmented 3.1 MB TDMS 2.0 file.

```
BenchmarkDotNet=v0.12.1, OS=macOS 11.1 (20C69) [Darwin 20.2.0]
Intel Core i9-9980HK CPU 2.40GHz, 1 CPU, 16 logical and 8 physical cores
.NET Core SDK=5.0.101
[Host]        : .NET Core 5.0.1 (CoreCLR 5.0.120.57516, CoreFX 5.0.120.57516), X64 RyuJIT DEBUG
.NET Core 5.0 : .NET Core 5.0.1 (CoreCLR 5.0.120.57516, CoreFX 5.0.120.57516), X64 RyuJIT

Job=.NET Core 5.0  Runtime=.NET Core 5.0
```

| Method |     Mean |     Error |    StdDev | Ratio | RatioSD |
|--------|----------|-----------|-----------|-------|---------|
|          `TDMSReader` | 5.531 ms | 0.1049 ms | 0.0930 ms |  1.00 |    0.00 |
|      `FSharp.Data.Tdms` synchronously | 1.962 ms | 0.0378 ms | 0.0435 ms |  0.35 |    0.01 |
| `FSharp.Data.Tdms` asynchronously  | 5.061 ms | 0.0503 ms | 0.0471 ms |  0.91 |    0.02 |

### Medium-sized file

This benchmark reads a channel of 43,200 strings from a segmented 138.1 MB TDMS 2.0 file.

```
BenchmarkDotNet=v0.12.1, OS=macOS 11.1 (20C69) [Darwin 20.2.0]
Intel Core i9-9980HK CPU 2.40GHz, 1 CPU, 16 logical and 8 physical cores
.NET Core SDK=5.0.101
[Host]        : .NET Core 5.0.1 (CoreCLR 5.0.120.57516, CoreFX 5.0.120.57516), X64 RyuJIT DEBUG
.NET Core 5.0 : .NET Core 5.0.1 (CoreCLR 5.0.120.57516, CoreFX 5.0.120.57516), X64 RyuJIT

Job=.NET Core 5.0  Runtime=.NET Core 5.0
```
| Method |     Mean |    Error |   StdDev | Ratio |
|--------|----------|----------|----------|------|
|          `TDMSReader` | 12.334 s | 0.2287 s | 0.2139 s | 1.00 |
|      `FSharp.Data.Tdms` synchronously |  4.400 s | 0.0370 s | 0.0328 s | 0.36 |
| `FSharp.Data.Tdms` asynchronously |  6.797 s | 0.0981 s | 0.0918 s | 0.55 |

### Large file

This benchmark reads a channel of 779,297 double-precision floating points from a segmented 1.54 GB TDMS 2.0 file.

```
BenchmarkDotNet=v0.12.1, OS=macOS 11.1 (20C69) [Darwin 20.2.0]
Intel Core i9-9980HK CPU 2.40GHz, 1 CPU, 16 logical and 8 physical cores
.NET Core SDK=5.0.101
[Host]        : .NET Core 5.0.1 (CoreCLR 5.0.120.57516, CoreFX 5.0.120.57516), X64 RyuJIT DEBUG
.NET Core 5.0 : .NET Core 5.0.1 (CoreCLR 5.0.120.57516, CoreFX 5.0.120.57516), X64 RyuJIT

Job=.NET Core 5.0  Runtime=.NET Core 5.0
```
| Method |    Mean |    Error |   StdDev | Ratio |
|--------|---------|----------|----------|-------|
|          `TDMSReader` | 2.145 s | 0.0242 s | 0.0214 s |  1.00 |
|      `FSharp.Data.Tdms` synchronously | 1.103 s | 0.0123 s | 0.0103 s |  0.52 |
| `FSharp.Data.Tdms` asynchronously | 1.953 s | 0.0167 s | 0.0157 s |  0.91 |
