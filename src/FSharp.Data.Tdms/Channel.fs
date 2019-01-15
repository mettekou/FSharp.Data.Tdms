namespace FSharp.Data.Tdms

open System
open System.IO

type Channel = {
  Type : Type
  Properties : Map<string, Value>
  Read : BinaryReader -> obj
  RawDataBlocks : (uint64 * uint64) list
 }

module Channel =

  let merge { Properties = ps; RawDataBlocks = bs } { Read = r'; Type = ty'; Properties = ps';  RawDataBlocks = bs' } =
    { Type = ty'; Properties = Map.fold (fun ps k v -> Map.add k v ps) ps' ps; Read = r'; RawDataBlocks = List.append bs bs'  }
  
  let propertyValue<'T> name channel =
    Map.tryFind name channel.Properties |> Option.bind (fun v -> if (Type.system v.Type).IsAssignableFrom(typeof<'T>) then Some(box v :?> 'T) else None)
  
  let rawData<'T> (reader : BinaryReader) channel =
    if typeof<'T>.IsAssignableFrom(channel.Type) then
          Some [| for (p, l) in channel.RawDataBlocks do
                    reader.BaseStream.Seek(int64 p, SeekOrigin.Begin) |> ignore
                    for _ in 1uL..l ->
                      channel.Read reader :?> 'T |]
      else None