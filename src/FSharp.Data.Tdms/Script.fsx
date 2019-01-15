#r "C:/TDLM/GreyMarket/FSharp.Data.Tdms/src/FSharp.Data.Tdms/bin/Debug/netstandard2.0/FSharp.Data.Tdms.dll"

open System
open System.IO
open FSharp.Data.Tdms

let path = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), @"AVA0103F_04\18092200001\Log_2018-10-17_17·04·08__SPCurrSwp.tdms")
let resultsPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "Results.csv")

let stopWatch = System.Diagnostics.Stopwatch.StartNew()
let file = File.read path
let results = file |> File.rawData<float> "Calculated channels" "M_CAL_FLW_Diff"

stopWatch.Stop()

let time = stopWatch.Elapsed.TotalMilliseconds