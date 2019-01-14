#load "Type.fs"
#load "Reads.fs"
#load "Value.fs"
#load "Segment.fs"
#load "Index.fs"

open System
open System.IO
open FSharp.Data.Tdms

let path = @"%userprofile%\AVA0103F_04\18092200001\Log_2018-10-17_17·04·08__SPCurrSwp.tdms"
//let path = @"/Users/mettekou/Documents/Projecten/mettekou/FSharp.Data.Tdms/Extended.tdms"

let results = Index.read path |> Index.rawDataFor "/'Calculated channels'/'M_CAL_FLW_Diff'" |> Array.map (sprintf "%A" >> (fun s -> s.Replace(".", ","))) |> String.concat Environment.NewLine

File.WriteAllText(@"C:\Users\Dylan.meysmans\Desktop\Results.csv", results)