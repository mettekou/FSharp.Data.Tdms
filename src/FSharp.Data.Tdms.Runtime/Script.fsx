#r "C:/TDLM/GreyMarket/FSharp.Data.Tdms/src/FSharp.Data.Tdms.Runtime/bin/Debug/netstandard2.0/FSharp.Data.Tdms.Runtime.dll"

open System
open System.IO
open FSharp.Data.Tdms

let path = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), @"AVA0103F_04\18092200001\Log_2018-10-17_17·04·08__SPCurrSwp.tdms")
let resultsPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "Results.csv")

let getDuplicates xs = xs |> List.groupBy id
                          |> List.choose( fun( key, set ) ->
                               if set.Length > 1
                                  then Some key
                                  else None )

//let stopWatch = System.Diagnostics.Stopwatch.StartNew()
let file = File.read path
let dups = file.Groups |> Map.toList |> List.collect (fun (_, g) -> Map.toList g.Channels |> List.map fst) |> getDuplicates
//let results = file |> File.rawData<float> "Calculated channels" "M_CAL_FLW_Diff"

//stopWatch.Stop()

//let time = stopWatch.Elapsed.TotalMilliseconds