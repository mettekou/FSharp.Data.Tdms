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

BenchmarkDotNet=v0.13.2, OS=opensuse-tumbleweed 20221224
AMD Ryzen 9 5950X, 1 CPU, 32 logical and 16 physical cores
.NET SDK=7.0.101
  [Host]   : .NET 7.0.1 (7.0.122.56804), X64 RyuJIT AVX2 DEBUG
  .NET 7.0 : .NET 7.0.1 (7.0.122.56804), X64 RyuJIT AVX2

Job=.NET 7.0  Runtime=.NET 7.0

```

| Method              |       Mean |    Error |   StdDev | Ratio |
| ------------------- | ---------: | -------: | -------: | ----: |
| TDMSReader          | 1,729.1 μs |  9.91 μs |  9.27 μs |  1.00 |
| FSharpDataTdms      |   737.0 μs |  1.72 μs |  1.43 μs |  0.43 |
| FSharpDataTdmsAsync |   910.2 μs | 17.96 μs | 20.68 μs |  0.52 |

### Medium-sized file

This benchmark reads a channel of 43,200 strings from a segmented 138.1 MB TDMS 2.0 file.

```ini

BenchmarkDotNet=v0.13.2, OS=opensuse-tumbleweed 20221224
AMD Ryzen 9 5950X, 1 CPU, 32 logical and 16 physical cores
.NET SDK=7.0.101
  [Host]   : .NET 7.0.1 (7.0.122.56804), X64 RyuJIT AVX2 DEBUG
  .NET 7.0 : .NET 7.0.1 (7.0.122.56804), X64 RyuJIT AVX2

Job=.NET 7.0  Runtime=.NET 7.0

```

| Method              |    Mean |    Error |   StdDev | Ratio |
| ------------------- | ------: | -------: | -------: | ----: |
| TDMSReader          | 9.001 s | 0.0948 s | 0.0886 s |  1.00 |
| FSharpDataTdms      | 2.657 s | 0.0113 s | 0.0088 s |  0.29 |
| FSharpDataTdmsAsync | 2.791 s | 0.0329 s | 0.0291 s |  0.31 |

### Large file

This benchmark reads a channel of 779,297 double-precision floating points from a segmented 1.54 GB TDMS 2.0 file.

```ini

BenchmarkDotNet=v0.13.2, OS=opensuse-tumbleweed 20221224
AMD Ryzen 9 5950X, 1 CPU, 32 logical and 16 physical cores
.NET SDK=7.0.101
  [Host]   : .NET 7.0.1 (7.0.122.56804), X64 RyuJIT AVX2 DEBUG
  .NET 7.0 : .NET 7.0.1 (7.0.122.56804), X64 RyuJIT AVX2

Job=.NET 7.0  Runtime=.NET 7.0

```

| Method              |       Mean |    Error |   StdDev | Ratio | RatioSD |
| ------------------- | ---------: | -------: | -------: | ----: | ------: |
| TDMSReader          | 1,028.7 ms | 12.10 ms | 10.11 ms |  1.00 |    0.00 |
| FSharpDataTdms      |   533.7 ms |  4.77 ms |  4.46 ms |  0.52 |    0.01 |
| FSharpDataTdmsAsync |   697.8 ms | 13.41 ms | 31.35 ms |  0.67 |    0.03 |
