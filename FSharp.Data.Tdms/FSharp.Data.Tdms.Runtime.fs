namespace FSharp.Data

// Put any runtime constructs here
// type DataSource(filename:string) =
//     member this.FileName = filename

open FSharp.Data.Tdms

module Helpers =

    type RawDataHelper =

        static member RawData<'T>(group, channel, index) =
            File.tryGetRawData<'T> group channel index
            |> Option.defaultValue [||]

    let rawData ty group channel index =
        let generic =
            typeof<RawDataHelper>.GetMethod "RawData"

        let concrete = generic.MakeGenericMethod [| ty |]
        concrete.Invoke(null, [| group; channel; index |])
